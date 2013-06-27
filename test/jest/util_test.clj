(ns jest.util-test
  (:use midje.sweet
        jest.testutils
        [clojure.test :only [deftest]])
  (:require [jest.util :as util]))

(fact "derefable? should return true for a derefable"
  (util/derefable? (ref nil)) => true)

(fact "derefable? should return false for non-derefable"
  (util/derefable? nil) => false)

(fact "maybe-deref should behave as the identity
function for non-derefables"
  (util/maybe-deref nil) => nil
  (util/maybe-deref 10) => 10
  (util/maybe-deref 'a) => 'a
  (util/maybe-deref [:a 2 4]) => [:a 2 4])

(fact "maybe-deref should behave as the deref function for derefables"
  (util/maybe-deref (ref nil)) => nil
  (util/maybe-deref (ref 10)) => 10
  (util/maybe-deref (ref 'a)) => 'a
  (util/maybe-deref (ref [:a 2 4])) => [:a 2 4])

(fact "plural should plurarize most english words"
  (util/plural "baby") => "babies"
  (util/plural "brains") => "brains"
  (util/plural "shoe") => "shoes")

(fact "ncomp with n = 0 should behave like
the identity function"
  (let [n0 (util/ncomp anything 0)]
    (n0 nil) => nil
    (n0 10) => 10
    (n0 'a) => 'a
    (n0 [:a 2 4]) => [:a 2 4]))

(fact "ncomp with n < 0 should behave like
the identity function"
  (let [n-neg (util/ncomp anything -7)]
    (n-neg nil) => nil
    (n-neg 10) => 10
    (n-neg 'a) => 'a
    (n-neg [:a 2 4]) => [:a 2 4]))

(fact "ncomp should be able to compose incrementors
and decrementors"
  (let [inc10 (util/ncomp inc 10)
        dec5 (util/ncomp dec 5)]
    (inc10 10) => 20
    (inc10 -10) => 0
    (dec5 10) => 5
    (dec5 -10) => -15
    (inc10 (dec5 5)) => 10))

(fact "offset-vec should return an empty map for empty vectors"
  (util/offset-vec [] 42) => {})

(fact "offset-vec should return an explicitly indexed version
of vec when offset = 0"
  (util/offset-vec [:0 :1 :2 :3] 0) => {0 :0
                                        1 :1
                                        2 :2
                                        3 :3})

(fact "offset-vec should return an offsetted explicitly
indexed version of vec"
  (util/offset-vec [:0 :1 :2 :3] 4) => {4 :0
                                        5 :1
                                        6 :2
                                        7 :3})

(fact "offset-map should return the empty map if the
input is also an empty map"
  (util/offset-map {} 42) => {})

(fact "offset-map should return a correctly offset map"
  (util/offset-map {:0 :a :1 :b :2 :c :3 :d} 4)
    => {4 :a 5 :b 6 :c 7 :d})

(fact "two-step-map should return an empty map if either of
the input maps is empty"
  (util/two-step-map {} {}) => {}
  (util/two-step-map {:a :a :b :b} {}) => {}
  (util/two-step-map {} {:a :a :b :b} ) => {})

(fact "two step map should contain linked entries"
  (util/two-step-map {:a :1 :b :2 :c :3} {:1 :a :23 :b :c :3})
    => {:a :a}
  (util/two-step-map {:a :1 :b :2 :c :3} {:12 :x :24 :a :c :c})
    => {}
  (util/two-step-map {:a :a :b :b :c :c} {:a :a :b :b :c :c})
    => {:a :a :b :b :c :c})

(fact "Hyphenate-keyword."
  (util/hyphenate-keywords) => (keyword "")
  (util/hyphenate-keywords :a) => :a
  (util/hyphenate-keywords :a :b) => :a-b
  (util/hyphenate-keywords :c :b :a) => :c-b-a)

(fact "group-seq"
  (let [in-seq (take 5 (range))
        grouped (util/group-seq in-seq
                           {:odd odd?
                            :even even?
                            :numbers number?})]
    (:numbers grouped) => in-seq
    (:even grouped) => '(0 2 4)
    (:odd grouped) => '(1 3)))

(fact "The hue difference between 0.1*pi and pi is 0.9*pi"
      (util/angle-difference (* 0.1 Math/PI) Math/PI)
      => (roughly (* 0.9 Math/PI) 0.01))

(fact "The hue difference between 0.1*pi and 1.9*pi is 0.2*pi"
      (util/angle-difference (* 0.1 Math/PI) (* 1.9 Math/PI))
      => (roughly (* 0.2 Math/PI) 0.01))
