(ns odie.sokoban.cli.init
  (:require [odie.sokoban.utils :as u]
            [me.raynes.fs :as fs]
            [clojure.pprint :as pprint]
            [odie.sokoban.cli :as cli]
            [colorize.core :refer [blue]]
            [odie.sokoban.aws-api :as api]
            [odie.sokoban.aws-utils :as au]
            [odie.sokoban.globals :as g]
            [odie.sokoban.compose-file :as compose-file]
            [odie.sokoban.shell :refer [shell]]))

(defn wiz-use-existing-image-repo [compose-service-entry sk-svr]
  (let [app-name (:app-name @g/app-context)
        svr-name (name (:name sk-svr))
        repo-name (format "%s/%s" app-name svr-name)
        reply (api/invoke :DescribeRepositories {:repositoryNames [repo-name]})]

        (if (au/aws-error? reply)
          (throw (ex-info "Unable to describe respository" {:repositoryNames [repo-name]
                                                            :error reply}))
          ;; Use the existing Arn
          (assoc sk-svr :image-repo (get-in reply [:repositories 0 :repositoryArn])))))

(defn wiz-create-image-repo [compose-service-entry sk-svr]
  (let [app-name (:app-name @g/app-context)
        svr-name (:name sk-svr)
        repo-name (format "%s/%s" app-name svr-name)
        _ (println "Creating a repo named" repo-name)

        ;; Try to create the repo now
        create-reply (api/invoke :CreateRepository
                                 {:repositoryName repo-name
                                  :tags (au/->tags {:sokoban-application app-name
                                                    :sokoban-service svr-name})})]
    ;; Did we succeed?
    (if (au/aws-error? create-reply)
      ;; We're ready to deal with errors where the repo already exists
      (if (not= (get-in create-reply [:__type]) "RepositoryAlreadyExistsException")
        ;; If it's not an error we're expecting, throw the error now
        (throw (ex-info "Repo creation failed" {:msg "Repo creation failed"
                                                :error create-reply}))

        ;; The repo already exists, get the Arn for the repo now
        (wiz-use-existing-image-repo compose-service-entry sk-svr))

      (assoc sk-svr :image-repo (get-in create-reply [:repository :repositoryArn])))))


(defn wiz-set-image-repo- [compose-service-entry sk-svr]
  (println "Looks like we need to have a docker image repo for the service named:"
           (:name sk-svr))
  (let [selection (cli/choice nil [{:text "Use an existing ECR repo"
                                    :action :use-existing-repo}
                                   {:text "Create a new repo for me"
                                    :action :create-new-repo}
                                   {:text "Enter a custom repo URI"
                                    :action :custom-repo-uri}
                                   ])]
    (case (:action selection)
      :use-existing-repo
      (wiz-use-existing-image-repo compose-service-entry sk-svr)

      :create-new-repo
      (wiz-create-image-repo compose-service-entry sk-svr)

      :custom-repo-uri
      (let [repo-path (cli/get-user-input "Please enter the uri of the repo")]
        (assoc sk-svr :image-repo repo-path)))))

(defn wiz-set-image-repo [compose-service-entry sk-svr]
  (if-not (and
           (:build compose-service-entry)
           (not (:image-repo sk-svr)))
    sk-svr

    (wiz-set-image-repo- compose-service-entry sk-svr)))

(defn sk-svr-wizard [compose-service-kv]
  (let [svr-name (name (first compose-service-kv))
        svr (second compose-service-kv)]
    (->> {:name svr-name}
         (wiz-set-image-repo svr))))

(defn sk-app-wizard []
  (println "Okay! Let's get started!")
  (let [app-name (cli/get-user-input "What would you like to name this app?")]
    (swap! g/app-context assoc :app-name app-name)
    {:app-name app-name}))

(defn init- [{:keys [compose-file-data] :as params}]
  (let [ident-reply (api/invoke :GetCallerIdentity {})
        id (->> ident-reply
                :Arn
                au/arn->map
                :resource-id)]
    (println (format "Signed in as %s for account %s\n" (blue id) (blue (str (:Account ident-reply))))))

  (let [app (sk-app-wizard)
        svrs (into []
              (for [compose-service-kv (:services compose-file-data)]
                (sk-svr-wizard compose-service-kv)))
        app (-> app
                (assoc :services svrs))]

    (clojure.pprint/pprint app)
    (u/write-edn (:project-file params) app)))

(defn init [params]
  ;; Verify compose file can be found
  (let [compose-file (fs/file "compose.yml")
        project-file (fs/file "../.sokoban")]

    (when (and (cli/exists?-or-print-error compose-file "Compose file cannot be found: %s")
               (cli/absent?-or-print-error project-file "Sokoban project already initialized"))

      ;; Actually perform the work
      (init- (into params {:compose-file-data (compose-file/read-compose-file compose-file)
                           :project-file project-file}))))
  )

(def dispatch-table
  [{:cmds ["init"] :cmds-opts [] :fn init}])


(comment

  (alter-var-root #'cli/*in-repl* (constantly true))

  (fs/with-mutable-cwd
    (fs/chdir (fs/expand-home "~/dev/kengoson/backend"))
    (init {}))

  (shell {:dir (fs/expand-home "~/dev/kengoson/backend")}
         "docker build . -t some/tag --file Dockerfile")

  (shell {:dir (fs/expand-home "~/dev/kengoson/backend")}
         "sleep 2")

  )
