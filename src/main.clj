(ns main
  (:use clojure.pprint)
  (:require
    [invoice-spec :as model]
    [clojure.spec.gen.alpha :as gen]
    [clojure.data.json :as json]
    [clojure.string :as cstr]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))

;; Problem 1
(def invoice (edn/read-string (slurp "../invoice.edn")))

(defn has-iva? [{:tax/keys [category rate]
                 :as tax}]
  (and (= category :iva) (= rate 19)))

(defn has-retention? [{:retention/keys [category rate]
                       :as retention}]
  (and (= category :ret_fuente) (>= rate 1)))

(defn invoice-filter [invoice]
  (->>
    (:invoice/items invoice)
    (filter (fn [item]
              (let [has-iva (some #(has-iva? %) (:taxable/taxes item))
                    has-retention (some #(has-retention? %) (:retentionable/retentions item))]
                (if has-iva
                  (not has-retention)
                  has-retention))))))
;; Result
(def problem1 (invoice-filter invoice))

;; Problem 2
(def ^java.text.SimpleDateFormat date-format
  (java.text.SimpleDateFormat. "dd/MM/yyyy"))

;; params  ->
;;   key   --> k
;;   value --> v
(defn values-fn [k v]
  (->
    k
    (case
      :invoice/issue-date (.parse date-format v)
      :tax/category (keyword (cstr/lower-case v))
      :tax/rate (double v)
      v)))

;; params  ->
;;   key   --> k
(defn keys-fn [k]
  (->
    k
    (case
      ("issue_date" "order_reference" "payment_date" "payment_means" "payment_means_type" "number" "items" "customer") (keyword "invoice" (cstr/replace k #"_" "-"))
      ("company_name" "email") (keyword "customer" (cstr/replace k #"company_" ""))
      ("price" "quantity" "sku" "taxes") (keyword "invoice-item" k)
      ("tax_category" "tax_rate") (keyword "tax" (cstr/replace k #"tax_" ""))
      (keyword k))))


(defn generate-invoice [file]
  (let [json-data (slurp file)
        invoice (json/read-str json-data :key-fn keys-fn :value-fn values-fn)]
    (:invoice invoice)))

;; Result
(def problem2 (generate-invoice "../invoice.json"))

(println (apply str (repeat 40 "-")))
(println "Problem 1 Thread-last Operator ->>")
(println (apply str (repeat 40 "-")))
(pprint problem1)

(println (apply str (repeat 40 "-")))
(println "Problem 2: Core Generating Functions")
(println (apply str (repeat 40 "-")))
(println (s/valid? ::model/invoice problem2))
(pprint (gen/generate (s/gen ::model/invoice)))