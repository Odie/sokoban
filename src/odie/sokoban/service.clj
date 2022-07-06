(ns odie.sokoban.service
  (:require [clj-yaml.core :as yml]
            ;; [cfn-yaml.core :as cfn-yml]
            ;; [yaml.core :as yaml]
            [odie.sokoban.globals :as g]
            [clojure.java.io :as io]
            [charred.api :as charred]
            [me.raynes.fs :as fs]
            ;; [clojure.java.shell :as sh]

            [odie.sokoban.utils :as u]
            [clojure.string :as str]
            [odie.sokoban.shell :refer [shell]]

            [odie.sokoban.aws-api :as api]
            [odie.sokoban.aws-utils :as au]))

(defn build-docker-image-cmd [{:keys [dockerfile name]}]
  (assert name ":name for the docker image must be specified in format of 'name:tag'")
  (let [cmd-params (concat [(u/interp "-t #{name}")]
                           (->> (u/ensure-in-seq-coll dockerfile)
                                (map #(format "-f %s" %))))]
    (str/join " "
              (concat [(u/interp "docker build .")] cmd-params))))

(defn build-docker-image [{:keys [working-dir] :as params}]
  (->> (build-docker-image-cmd params)
       (shell {:dir (fs/expand-home working-dir)})))

(defn repo-root-uri
  [{:keys [account region]}]
  (u/interp "#{account}.dkr.ecr.#{region}.amazonaws.com"))

(defn repo-uri
  [{:keys [account region repo-name]}]
  (u/interp "#{account}.dkr.ecr.#{region}.amazonaws.com/#{repo-name}"))

(defn repo-image-uri
  [{:keys [account region repo-name tag]}]
  (u/interp "#{account}.dkr.ecr.#{region}.amazonaws.com/#{repo-name}:#{tag}"))

(defn decode-base64 [to-decode]
  (String. (.decode (java.util.Base64/getDecoder) to-decode)))

(defn fetch-ecr-authorization-token
  "Fetch and decode a token/password to be used for pushing an image to ECR"
  [account-id]
  (au/aws-when-let*
   [res (api/invoke :GetAuthorizationToken {:registryIds [account-id]})]
   (-> res
       (get-in [:authorizationData 0 :authorizationToken])
       decode-base64
       (str/split #":")
       second)))

(defn upload-docker-image [local-name remote-target]
  (au/aws-when-let*
   [account-id (:account-id @g/app-context)

    token (fetch-ecr-authorization-token account-id)

    root (repo-root-uri remote-target)
    repo (repo-uri remote-target)
    push-target (repo-image-uri remote-target)]


   ;; Tell docker how to authenticate itself to ECR
   (shell (u/interp "docker login -u AWS -p #{token} #{root}"))

   ;; Tag the image with the URI we're going to push to
   (shell (u/interp "docker tag #{local-name} #{push-target}"))

   ;; Actually push the image
   (shell (u/interp "docker push #{push-target}"))))

(defn build-image-and-upload
  "`working-dir` which directory should we be in to build the image?
  `local-name` tag the image with this name:tag locally after it's built.
  `remote-target` a map that describes where to upload the image. It should contain a map
  in the following format:
  {
    :account \"string: AWS account id\"
    :region \"name of the region of the target repo\"
    :repo-name \"name of the repo to up to\"
    :tag \"tag to set on the image\"
  }
  "
  [& {:keys [working-dir dockerfile local-name remote-target]}]

  (build-docker-image {:working-dir working-dir :name local-name :dockerfile dockerfile})
  (upload-docker-image local-name remote-target)
  :done)

(comment

  (let [image-ref {:account (:account-id @g/app-context)
                   :region "us-west-1"
                   :repo-name "some/repo"
                   :tag "dev-0.0.5"}]
    (println (repo-root-uri image-ref))
    (println (repo-uri image-ref))
    (println (repo-image-uri image-ref)))

  (shell "ls -alh .git")

  (shell {:dir (fs/expand-home "~")}
         "pwd")

  (build-docker-image-cmd {:name "some/tag" :dockerfile "Dockerfile"})

  (build-docker-image-cmd {:name "some/tag" :dockerfile ["Dockerfile" "Dockerfile.prod"]})

  (build-docker-image-cmd {:working-dir "~/dev/kengoson/backend" :tag "some/tag" :dockerfile "Dockerfile"})

  (build-docker-image {:working-dir "~/dev/kengoson/backend" :tag "some/tag" :dockerfile "Dockerfile"})

  (build-image-and-upload
   :working-dir "~/dev/kengoson/backend"
   :dockerfile "Dockerfile"
   :local-name "kengoson/backend"
   :remote-target {:account (:account-id @g/app-context)
                   :region "us-west-1"
                   :repo-name "kengoson-backend"
                   :tag "latest"})

  )
