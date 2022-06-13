(ns odie.sokoban.cfn-yaml)


(defn !and [body]
  {"Fn::And" body})

(defn !equals [body]
  {"Fn::Equals" body})

(defn !if [body]
  {"Fn::If" body})

(defn !not [body]
  {"Fn::Not" body})

(defn !or [body]
  {"Fn::Or" body})

(defn !base64 [body]
  {"Fn::Base64" body})

(defn !cidr [body]
  {"Fn::Cidr" body})

(defn !find-in-map [body]
  {"Fn::FindInMap" body})

(defn !get-att [body]
  {"Fn::GetAtt" body})

(defn !get-AZs [body]
  {"Fn::GetAZs" body})

(defn !import-value [body]
  {"Fn::ImportValue" body})

(defn !join [body]
  {"Fn::Join" body})

(defn !select [body]
  {"Fn::Select" body})

(defn !split [body]
  {"Fn::Split" body})

(defn !sub
  ([target-str]
   {"Fn::Sub" target-str})
  ([target-str vars]
   {"Fn::Sub" [target-str vars]}))

(defn !transform [body]
  {"Fn::Transform" body})

(defn !ref [body]
  {"Fn::Ref" body})
