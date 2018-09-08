(ns wakana.core
  (:require [overtone.live :refer :all]))

;; Initialize Tempo
(def bpm (atom 140))
(def metro (metronome @bpm)) ;; (metro) -> current beat number
(def beat-dur (atom (/ 60.0 @bpm)))

(defn reset-bpm!
  [bpm-val]
  (reset! bpm bpm-val)
  (reset! beat-dur (/ 60.0 @bpm))
  (metro-bpm metro bpm-val))

(definst sin-wave [freq 440 attack 0.01 sustain @beat-dur release 0.01 vol 0.25] 
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (sin-osc freq)
     vol))

(defn sinw
  [music-note & opts]
  (let [tone (midi->hz (note music-note))]
    (apply sin-wave (cons tone opts))))

(def bass-range    (range (note :e2) (inc (note :c4))))
(def tenor-range   (range (note :c3) (inc (note :c5))))
(def alto-range    (range (note :f3) (inc (note :f5))))
(def soprano-range (range (note :c4) (inc (note :c6))))
(def human-range   (range (apply min bass-range)
                          (apply max soprano-range)))

(defn in-range?
  "Returns true if n is between min and max of nums, inclusive."
  [n nums]
  (let [[lo hi] (apply (juxt min max) nums)]
    (<= lo n hi)))

(def scale-degrees [:i :ii :iii :iv :v :vi :vii])

(defn scale-chords
  [tonic-key]
  (map chord
       (degrees->pitches scale-degrees :major tonic-key)
       [:major :minor :minor :major :major :minor :dim]))

(defn rand-chords
  [tonic-key]
  (let [chords (scale-chords tonic-key)]
    (repeatedly #(rand-nth chords))))

(defn- collectionize [x] (if (coll? x) x [x]))

(defn play-pitches-at
  [beat pitch-or-pitches & opts]
  (at beat
    (doseq [pitch (collectionize pitch-or-pitches)]
      (apply sinw pitch opts))))

(defn play-melody
  [melody]
  (let [beat (metro)]
    (play-pitches-at (metro beat) (first melody))
    (apply-by (metro (inc beat)) play-melody [(next melody)])))

(defn all-key-pitches
  "Given a collection of pitches, keep those in the provided key"
  [tonic-key pitches]
  (let [key-pitches (set (scale-field tonic-key))]
    (->> (collectionize pitches)
         (filter key-pitches))))

(defn consonant?
  [pitch chord]
  (let [chord-semitones (set (map #(mod % 12) chord))
        pitch-semitone (mod pitch 12)]
    (chord-semitones pitch-semitone)))

(defn nearby-consonant-pitches
  [pitch next-chord pitch-range]
  (let [near-range (range (- pitch 7) (+ pitch 8))]
    (->> near-range
         (filter #(in-range? % pitch-range))
         (filter #(consonant? % next-chord)))))

(defn smooth-melody [chords pitch-range]
  (let [init-state [(->> pitch-range
                         (filter #(consonant? % (first chords)))
                         (rand-nth))
                    (rest chords)]]
    (->> init-state
         (iterate (fn [[last-pitch chords]]
                    [(-> last-pitch
                         (nearby-consonant-pitches (first chords) pitch-range)
                         (rand-nth))
                     (rest chords)]))
         (map first))))

(defn smooth-harmony [chords melody pitch-range]
  (let [init-state [(->> pitch-range
                         (filter #(consonant? % (first chords)))
                         (filter #(not= (mod (first melody) 12)
                                        (mod % 12)))
                         (rand-nth))
                    (rest chords)
                    (rest melody)]]
    (->> init-state
         (iterate (fn [[last-pitch chords melody]]
                    [(as-> last-pitch $
                           (nearby-consonant-pitches $ (first chords) pitch-range)
                           (filter #(not= (mod (first melody) 12)
                                          (mod % 12))
                                   $)
                           (rand-nth $))
                     (rest chords)
                     (rest melody)]))
         (map first))))

(defn play-piece
  [chords melody]
  (let [beat (metro)]
    (play-pitches-at (metro beat) (first chords) :vol 0.20 :sustain (* 4 @beat-dur))
    (play-pitches-at (metro beat) (first melody) :vol 0.35 :sustain (* 4 @beat-dur))
    (apply-by (metro (+ 4 beat))
              play-piece
              [(rest chords) (rest melody)])))

(defn gen-piece
  []
  (let [[a-section b-section] (split-at 4 (take 8 (rand-chords :c4)))
        chords (cycle (concat a-section a-section b-section a-section))
        melody (smooth-melody chords soprano-range)
        harmony (smooth-harmony chords melody alto-range)]
    (play-piece harmony melody)))

(defn smooth-counterpoint [chords melody pitch-range]
  (let [init-state [(->> pitch-range
                         (filter #(consonant? % (first chords)))
                         (shuffle)
                         (take 2))
                    (rest chords)
                    (rest melody)]]
    (->> init-state
         (iterate
           (fn [[last-pitch chords melody]]
             [(->> (nearby-consonant-pitches (second last-pitch) (first chords) pitch-range)
                   (filter #(not= (first melody) %))
                   (shuffle)
                   (take 2))
              (rest chords)
              (rest melody)]))
         (map first))))

(defn play-counterpoint
  [accompaniment melody]
  (let [beat (metro)]
    (play-pitches-at (metro beat) (-> accompaniment ffirst) :vol 0.25 :sustain (* 2 @beat-dur))
    (play-pitches-at (metro (+ beat 2)) (-> accompaniment first second) :vol 0.25 :sustain (* 2 @beat-dur))
    (play-pitches-at (metro beat) (first melody) :vol 0.25 :sustain (* 4 @beat-dur))
    (apply-by (metro (+ 4 beat))
              play-counterpoint
              [(rest accompaniment) (rest melody)])))

(defn gen-counterpoint
  []
  (let [[a-section b-section] (split-at 4 (take 8 (rand-chords :c4)))
        chords (cycle (concat a-section a-section b-section a-section))
        melody (smooth-melody chords soprano-range)
        accompaniment (smooth-counterpoint chords melody alto-range)]
    (play-counterpoint accompaniment melody)))

(comment
  (gen-counterpoint)
  (stop))
