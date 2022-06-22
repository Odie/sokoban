(ns odie.sokoban.zipper
  "Provides a zipper that can walk through a mix of the frequently used
  clojure data collections."
  (:require [clojure.zip :as zip]))

(defn- coll-branch? [node]
  (or
   (vector? node)
   (list? node)
   (map? node)
   (set? node)))

(defn- coll-children [node]
  (seq node))

(defn- coll-make-node [old-node new-children]
  (into old-node new-children))

(defn coll-zipper [root]
  (zip/zipper coll-branch? coll-children coll-make-node root))
