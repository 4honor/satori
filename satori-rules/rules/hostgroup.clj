(ns hostgroup
  (:use riemann.streams
        [riemann.test :only [tests io tap]]))

(defmacro is? [re]
  `(re-matches ~re ~'host))

(defn host->group [ev]
  (let [host (:host ev)]
    (cond
      (is? #"^docker\d+") [:operation :lean-engine]
      (is? #"^api\d+")    [:operation :api]
      (is? #"^push\d+")   [:operation :push]
      (is? #"^stats\d+")  [:operation :stats]
      :else               [:operation])))

(def hostgroup-definition (atom {}))

(defn defhostgroup
  [group nodes]
  (swap! hostgroup-definition assoc group (set nodes)))

(defn where-hostgroup
  [group & children]
  (where ((@hostgroup-definition group) (:host event))
    (apply sdo children)))


#_(this is an example

(defhostgroup :web ["web1" "web2" "web3"])

(def foo-bar-rules
  (where-hostgroup :web
    (your rules)))

)

; ------------------------------------------------------------------
(tests
  (hostgroup/defhostgroup :test ["host1" "host2"])
  (deftest hostgroup-test
    (let [s (hostgroup/where-hostgroup :test (tap :where-hostgroup))
          rst (inject! [s] [{:host "host1" :service "bar" :metric 10},
                            {:host "host2" :service "quux" :metric 20},
                            {:host "host3" :service "bar" :metric 80}])]
      (is (= [{:host "host1" :service "bar" :metric 10},
              {:host "host2" :service "quux" :metric 20}] (:where-hostgroup rst))))))
