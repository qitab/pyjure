(ns pyjure.environment
  (:require [clojure.core :as c]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math])
  (:use [clojure.core.match :only [match]]
        [pyjure.debug]
        [pyjure.utilities]
        [pyjure.names]
        [pyjure.mop]))


;;; Runtime and Compile-time environments

;; a scope at runtime
(defrecord RuntimeScope
    [^clojure.lang.PersistentVector values
     parent ;; a parent RuntimeScope, or nil if top-level
     ^clojure.lang.PersistentVector names]) ;; nil if no introspection

(defrecord CompileTimeEnvironment
    [lexical-scope ;; nil if at top-level
     toplevel-scope])

;;; a scope at compile-time is just a map from string to CompileTimeBinding, and a parent
(defrecord CompileTimeScope
    [bindings ;; a map from name to binding
     parent]) ;; nil if top-level

(defn scope-toplevel? [scope]
  ;; works for both RuntimeScope and CompileTimeScope
  (nil? (:parent scope)))

;; flags for a CompileTimeBinding
(declare +ctb-bits+)

(defn expand-ctb-mask [spec bits]
  (match [spec]
    [_ :guard keyword?] (or (get bits spec) ($syntax-error spec "ctb bit ~s not found" [:spec spec]))
    [(['or & ms] :seq)] (apply bit-or (map #(expand-ctb-mask % bits) ms))
    [(['and & ms] :seq)] (apply bit-and (map #(expand-ctb-mask % bits) ms))
    [(['not m] :seq)] (bit-not (expand-ctb-mask m bits))))

(defmacro ctb-mask
  ([spec] (expand-ctb-mask spec +ctb-bits+))
  ([flags spec] `(bit-and ~flags (ctb-mask ~spec))))

(defmacro def-ctb-bits [specs]
  (loop [shift 0 mask 1 specs specs bits {} rev {}]
    (match [specs]
      [_ :guard empty?] `(do (def +ctb-bits+ '~bits) (def +ctb-bit-name+ '~rev))
      [[[name combo] & more]]
      (let [flags (expand-ctb-mask combo bits)]
        (recur shift mask more (conj bits [name flags]) (conj rev [flags name])))
      [[name & more]]
      (if (>= shift 31) (throw (Exception. "Only 31 bits allowed in a defbits"))
          (recur (inc shift) (* 2 mask) more (conj bits [name mask]) (conj rev [mask name]))))))

(def-ctb-bits
  [:constant  ; is it a compile-time constant? (NB: it's always immutable at runtime)
   :typed  ; was the type specified?
   :dynamic  ; is it a dynamic variable? (like a Clojure var, a CL special variable)
   :global  ; was it declared global?
   :nonlocal  ; was it declared nonlocal?
   :class  ; is is known to be class-scoped? (TODO: handle class bindings)
   :parameter  ; it is a parameter?
   :def  ; it is a def target?
   :call-macro ;; bound as a call-macro
   :decorator-macro ;; bound as a decorator-macro
   :with-macro ;; bound as a with-macro
   :place-macro ;; bound as a place-macro
   :target  ; bound as target to some "assignment" var = value
   :bound-before-use  ; was it bound before it's used?
   :referenced ; was the variable referenced in the current scope?
   :call-referenced ; used as function
   :decorator-referenced ;; used as decorator
   :with-referenced ;; used as a with annotation
   :expanded ;; was previously resolved as a macro
   :found ;; for updated-mask only, found in the current scope
   [:annotated (or :typed :dynamic :constant)]
   [:forwarded (or :global :nonlocal)]
   [:defined (or :parameter :def :class)] ;; defined as a regular variable *in this scope*
   [:used (or :referenced :call-referenced :decorator-referenced :with-referenced :expanded)] ;; used in any way
   [:macro (or :call-macro :decorator-macro :with-macro :place-macro)]]) ;; bound as any kind of macro in this scope

(defmacro mask? [flags mask] `(pos? (bit-and ~flags ~mask)))
(defmacro ctb-mask? [flags mask] `(mask? ~flags (ctb-mask ~mask)))
(defmacro ctb-flags? [binding mask] `(ctb-mask? (:flags ~binding) ~mask))

(defmacro ctb-flags-assoc
  ([binding flags] `(update-in binding [:flags] #(bit-or % (ctb-mask ~flags))))
  ([binding mask flags] `(update-in binding [:flags] #(bit-or (bit-and % (bit-not mask)) (ctb-mask ~flags)))))

(defmacro ctb-bit-name
  ([bit] `(-ctb-bit-name ~bit))
  ([flags mask] `(ctb-bit-name (bit-and ~flags (ctb-mask ~mask)))))

(defmacro ctb-bit-names
  ([bit] `(-ctb-bit-names ~bit))
  ([flags mask] `(ctb-bit-names (bit-and ~flags (ctb-mask ~mask)))))

(defn first-bit [n]
  (if (zero? n) nil (loop [m 1] (if (pos? (bit-and m n)) m (recur (+ m m))))))

(defn -ctb-bit-name [bit]
  (or (get +ctb-bit-name+ bit) (get +ctb-bit-name+ (first-bit bit))))

(defn -ctb-bit-names [n]
  (loop [n n m 1 l []]
    (cond (zero? n) l
          (pos? (bit-and m n)) (recur (bit-and n (bit-not m)) (+ m m) (conj l (-ctb-bit-name m)))
          :else (recur n (+ m m) l))))

(defrecord CompileTimeBinding
    [^Integer flags
     value ;; the value, if it's a constant, or the expander if it's a macro
     type ;; the type, if known
     expansion-path]) ;; path to the macro that was expanded

(def null-CompileTimeBinding (map->CompileTimeBinding {}))

(defn check-flags [flags location]
  (when (and (ctb-mask? flags :nonlocal) (ctb-mask? flags :global))
    ($syntax-error location "Can't declare a variable both nonlocal and global"))
  (when (ctb-mask? flags :forwarded)
    (let [declname (ctb-bit-name flags :forwarded)]
      (when (ctb-mask? flags :defined)
        ($syntax-error location "Can't declare %a a %a" [declname (ctb-bit-name flags :defined)]))
      (when (ctb-mask? flags :annotated)
        ($syntax-error location "Can't annotate a %a variable as %a" [declname (ctb-bit-name flags :annotated)]))
      (when (ctb-mask? flags :macro)
        ($syntax-error location "Can't define a %a variable as a macro" [declname]))))
  (let [defined (ctb-mask flags :defined)]
    (when (pos? defined)
      (when-not (= defined (first-bit defined))
        ($syntax-error location "A variable can't be both %a and %a" (take 2 (ctb-bit-names defined))))
      (when (ctb-mask? flags :macro)
        ($syntax-error location "A variable can't be both %a and %a"
                       [(ctb-bit-name defined) (ctb-bit-name flags :macro)]))))
  (when (ctb-mask? flags :target) ;; restrictions not in Python
    (when (ctb-mask? flags :forwarded)
      ($syntax-error location "Can't bind a %a variable" [(ctb-bit-name flags :forwarded)]))
    (when (ctb-mask? flags :macro)
      ($syntax-error location "A variable can't be both bound and defined as a macro" [(ctb-bit-name flags :forwarded)])))
  flags)

(defn -compile-time-var [lexical-scope toplevel-scope name effect location levels-up nonlocal?]
  (let [;; The current scope
        scope (or lexical-scope toplevel-scope)
        ;; Is it the top-level scope?
        toplevel? (not lexical-scope)
        ;; The binding for the variable in this scope if it's known, or else a fresh binding.
        binding (get-in scope [:bindings name] null-CompileTimeBinding)
        ;; Is this variable being bound before it was ever used?
        bound-before-use (and (ctb-mask? effect :target)
                              (not (ctb-flags? binding (or :annotated :forwarded :defined :used))))
        ;; Add a synthetic effect for variables that are assigned before they are used.
        effect2 (if bound-before-use (bit-or effect (ctb-mask :bound-before-use)) effect)
        ;; Is this effect relevant to the local scope only?
        ;; excludes :target :bound-before-use :used :found
        local-effect? (ctb-mask? effect2 (or :annotated :forwarded :defined :macro :bound-before-use))
        ;; Is this scope the one that defines the current variable?
        found? (or toplevel? local-effect?
                   (ctb-flags? binding (or :annotated :defined :macro))
                   (and (ctb-flags? binding :target) (not (ctb-flags? binding :forwarded))))
        ;; New flags for the binding
        new-flags (check-flags effect2 location)]
    (letfn [(new-binding [path found-binding expanded?]
              (if expanded?
                (if (ctb-flags? binding :expanded)
                  (if (= (:expansion-path binding) path)
                    (assoc binding :flags new-flags)
                    ($syntax-error location "variable was previously expanded from a different source"))
                  (assoc binding
                    :flags (bit-or new-flags (ctb-mask :expanded))
                    :path path :value (:value found-binding)))
                (assoc binding :flags new-flags)))
            (new-bindings [new-binding]
              (assoc (:bindings scope) name new-binding))]
        (if found?
          (let [expanded? (and (ctb-mask? new-flags :macro) (ctb-mask? effect :used))]
            (if toplevel?
              (let [path [:global name]
                    new-binding (new-binding path binding expanded?)]
                [nil (->CompileTimeScope (new-bindings new-binding)) path new-binding])
              (let [path (if (zero? levels-up) [:local name] [:lexical levels-up name])
                    new-binding (new-binding path binding expanded?)]
                [(->CompileTimeScope (new-bindings new-binding) (:parent lexical-scope)) toplevel-scope path new-binding])))
          (let [;; Not found. Compute mask to recurse with.
                up-effect (bit-and effect2 (ctb-mask (or :target :used)))
                _ (assert (pos? up-effect))
                ;; Get information from upper scope.
                [up-lexical-scope up-toplevel-scope path up-binding]
                (cond
                 ;; If the variable wasn't declared, go to the next scope.
                 (not (ctb-mask? new-flags :global))
                 (-compile-time-var (:parent lexical-scope) toplevel-scope name up-effect location
                                    (inc levels-up) (or nonlocal? (ctb-flags? new-flags :nonlocal)))
                 ;; If the variable was declared global, go straight to top-level scope.
                 nonlocal?
                 ($syntax-error location "variable declared nonlocal but resolves to global")
                 :else
                 (-compile-time-var nil toplevel-scope name up-effect location -1 false))
                ;; Is this a macro-expansion?
                expanded? (ctb-flags? up-binding :macro)
                new-binding (new-binding path up-binding expanded?)
                new-bindings (new-bindings new-binding)]
            [(->CompileTimeScope new-bindings (:parent up-lexical-scope)) up-toplevel-scope
             path new-binding])))))

(defn compile-time-var
  "Given an environment, a variable name, and an effect on that variable,
lookup the variable by name, modify the environment to account for the effect, and
return a vector [updated-environment path binding] of the updated environment,
a path to the variable from current scope (assuming it will not be invalidated by a further
declaration), and the compile-time binding for the variable assuming the path is correct.
Issue a warning or an error as appropriate if the effect contradicts a previous reference,
reporting it as happening at given location.
The path can be a vector [:local ~name], [:lexical ~levels-up ~name], [:globals ~name].
An effect can be nil (no meta-modification), :global or :nonlocal (declaration),
:parameter or :def or :class (definition), :bind (binding with =), :reference (variable used),
:call-reference, :decorator-reference, :with-reference, :modify-reference ( +=, etc.)"
  [environment name effect location]
  (let [[lexical-scope toplevel-scope path binding]
        (-compile-time-var (:lexical-scope environment) (:toplevel-scope environment) name effect location 0 false)]
    [(->CompileTimeEnvironment lexical-scope toplevel-scope) path binding]))

;;; Initial global bindings
(def initial-globals (atom {}))

(defn register-global [name value]
  (swap! initial-globals #(assoc % name value)))
(defn register-globals [map]
  (swap! initial-globals #(merge % map)))

(defn map-map-values [f m]
  (into {} (map (fn [[name value]] [name (f value)]) m)))

(defn compile-time-bindings-for-known-constants [m]
  (map-map-values #(map->CompileTimeBinding {:constant true :value %}) m))

(defn initial-compile-time-globals []
  (compile-time-bindings-for-known-constants @initial-globals))

