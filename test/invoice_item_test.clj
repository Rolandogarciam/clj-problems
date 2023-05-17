(ns invoice-item-test
  (:require
    [invoice-item]
    [clojure.test :refer :all]))

(deftest invoice-with-half-price
  (let [invoice #:invoice-item {
                       :precise-quantity 1,
                       :precise-price 100,
                       :discount-rate 50}]
    (is (= (double 50) (invoice-item/subtotal invoice)))))

(deftest invoice-with-quantity-and-half-price
  (let [invoice #:invoice-item {
                       :precise-quantity 5,
                       :precise-price 200,
                       :discount-rate 50}]
    (is (= (double 500) (invoice-item/subtotal invoice)))))

(deftest invoices-with-no-discount
  (let [invoice #:invoice-item {
                       :precise-quantity 5,
                       :precise-price 100}]
    (is (= (double 500) (invoice-item/subtotal invoice)))))

(deftest invoice-with-no-price
  (let [invoice #:invoice-item {
                       :precise-quantity 5,
                       :precise-price 0,
                       :discount-rate 25}]
    (is (= (double 0) (invoice-item/subtotal invoice)))))

(deftest invoices-with-no-quantity
  (let [invoice #:invoice-item {
                       :precise-quantity 0,
                       :precise-price 50,
                       :discount-rate 75}]
    (is (= (double 0) (invoice-item/subtotal invoice)))))