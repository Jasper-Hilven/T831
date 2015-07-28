(comment "Basic-id")
(def id-generator (atom 0))
(defn id-from-char [argchar] {:char argchar})
(defn id-from-int  [argint]  {:int argint})
(defn get-next-id []
  (let [cur-val @id-generator]
    (do
      (swap! id-generator inc)
      cur-val)))
(def true-id  {:bool true})
(def false-id {:bool false})
(defmacro def-id [clojure-id]
  (let [lid (get-next-id)]
    `(do (def ~clojure-id ~lid) ~lid)))

(comment "Properties")
(def property-store (atom {}))
(defn add-property [target property value]
  (let [vvar (swap! property-store #(assoc-in % [target property] value))]
    target))

(comment "Namespacing")
(def-id root)
(defmacro def-child-id [father  clojure-id-path clojure-id]
  `(let [id-path#  (def-id ~clojure-id-path)
         id-id#    (def-id ~clojure-id)
         voidvar# (add-property  ~father id-path# id-id#)]
     ~clojure-id))
(def-child-id root root-base base-ns)

(comment "Listing")
(def-child-id base-ns base-list list-ns)
(def-child-id list-ns list-next-node next-node)
(def-child-id list-ns list-val node-val)
(def-child-id list-ns list-empty list-empty)
(defn register-list [cl-list]
  (if (empty? cl-list)
    list-empty
  (let [rest-registered (register-list (rest cl-list))
        currentNode (get-next-id)
        add-prop-val (add-property currentNode node-val (first cl-list))
        add-prop-next (add-property currentNode next-node rest-registered)]
    currentNode)))

(comment "Naming")
(def-child-id root root-name name-ns)
(def-child-id name-ns name-name name-prop)
(defn name-id [id name]
  (add-property id name-prop (register-list  (map id-from-char (seq (char-array name))))))
(defmacro def-child-id-named [father  clojure-id-path clojure-id
relation-name child-name]
  `(do (def-child-id ~father ~clojure-id-path ~clojure-id)
         (name-id ~clojure-id ~child-name)
     (name-id ~clojure-id-path ~relation-name)))
(register-list  (list 1))
(name-id root "root")
(name-id name-ns "naming")
(name-id root-name "naming")

(comment "Execution")
(def-child-id root root-exec exec-ns)
(def-child-id exec-ns exec-deepf deepfunction)
(def-child-id exec-ns exec-param-list param-list)
(def deep-functions (atom {}))
(defmacro build-deep-function [func-id clojure-func df-param-list]
  "param-list needs to be a list with unregistered params.
   The params will be add to the function as property."
  `(let [ voidvar#  (add-property ~func-id deepfunction deepfunction)
          voidvar2# (add-property ~func-id param-list ~df-param-list)
          voidvar3# (swap! deep-functions #(assoc % ~func-id
~clojure-func))] ~func-id ))

(build-deep-function 314 inc 4)


(def-child-id base-ns base-props  props-ns)

(def-child-id props-ns props-pget  pget)
(def-child-id props-ns props-pset  pset)
(def-child-id props-ns props-premove  premove)
(def-child-id props-ns props-pcontains  pcontains)
(def-child-id props-ns props-pequals  pequals)
(def-child-id base-ns base-bool  bool-ns)
(def-child-id base-ns base-int int-ns)


@deep-functions
@property-store
