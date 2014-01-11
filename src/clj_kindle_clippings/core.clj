(ns clj-kindle-clippings.core
  (:require [clojure.string :as str])
  (:use clojure.java.io))

(defrecord Info [page position date])
(defrecord Book [title author])
(defrecord Highlight [book info text])

(defn get-raw-highlights [txt]
  (let [raw (butlast (str/split txt #"=========="))]
    (map (fn [e]
           (if (.startsWith e "\r\n")
             (subs e 2)
             e)) raw)))

(defn get-title-author [raw]
  (let [[_ title author] (first (re-seq #"(.*)\s+\((.*)\)" raw))]
    (Book. title author)))

(defn get-info [raw]
  (let [[part1 part2 part3] (str/split raw #" \| ")
        page (last (str/split part1 #"pagina "))
        position (last (str/split part2 #"posizione "))
        date (last (str/split part3 #"Aggiunto in data "))]
    (Info. page position date)))

(defn get-highlight [raw]
  (let [[rbook rinfo _ text] (str/split raw #"\r\n")
        book (get-title-author rbook)
        info (try (get-info rinfo) (catch Exception e nil))]
    (Highlight. book info text)))

(defn get-highlights [txt]
  (map get-highlight (get-raw-highlights txt)))

;;(get-highlights (slurp "c.txt"))
