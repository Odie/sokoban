(ns odie.globals)

(defonce app-context (atom {}))
(defonce credentials (atom {}))
(defonce credentials-provider (atom {}))

(defn set-app-name! [s]
  (swap! app-context assoc :app-name s))

(defn set-account-id! [s]
  (swap! app-context assoc :account-id s))

(defn set-region! [s]
  (swap! app-context assoc :region s))
