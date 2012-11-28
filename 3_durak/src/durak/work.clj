(ns durak.work
  (:use [durak.core :only (run-game)] )
  (:use [clojure.set :only (difference union)] )
  (:use [clojure.pprint :only (pprint)] ))

(defmacro xor 
  ([] nil)
  ([a] a)
  ([a b]
    `(let [a# ~a
           b# ~b]
      (if a# 
        (if b# nil a#)
        (if b# b# nil)))))

;;; Your task is to write bot for playing card game "Durak": http://en.wikipedia.org/wiki/Durak
;;; Bot plays against another built-in bot (so 2 players).
;;; Bot is map that contains 2 functions: function to attack and function to defend.
;;; It must have following format: {:attack YOUR_ATTACK_FN  :defend YOUR_DEFEND_FN}
;;; Attack and defend functions are similar: they both take same arguments and both must return card.
;;; Attack function is called when your are an attacker and it needs to select 1 card from your hand and put it on the table.
;;; Note that if it is not the first card in current attack, it must have the same rank as one of the cards on the table.
;;; If you return nil it means that you don't want to attack and attack is finished.
;;; Defend function is called  when your are a defender and it needs to select 1 card from your hand and put it on the table.
;;; If you return nil it means you can't (or don't want to) defend and you take all cards from the table.

(def deck (set (for 
                 [suit [:spades :hearts :diamonds :clubs]
                    rank (range 6 15)]
                 {:suit suit :rank rank})))

(def cards-in-opps-hand (atom #{})) ; Cards, that we already know are in opp's hand
(def left-unknown (atom deck)) ; Draw deck + unknown opp's hand (still not seen in other words)

(def last-action (atom 0)) ; :defend-success / :defend-failure / :attack-success / :attack-failure
(def last-opps-hand-size (atom 6))
(def last-table (atom #{}))
(def last-hand-size (atom 6))

(def opps-hand-size (atom 6))
(def draw-size (atom 24))

; So, theese ^ atoms are representing the outer world state and are changed
;            |   at the beggining and at the end of each turn so as to make
;            |   bot more intelligent.

(defn can-beat [acard dcard trump]
  (if 
    (= (:suit acard) (:suit dcard))
    (> (:rank acard) (:rank dcard))
    (= (:suit acard) trump)))

(defn contains-equal? [place card] 
  ((set (map :rank place)) (:rank card)))

(defn can-be-beaten [card cardset trump]
  (some true? (map #(can-beat % card trump) cardset)))

(defn count-equal-non-trump [coll card trump]
  (count (filter #(and (= (:rank %) (:rank card)) (not= trump (:suit %))) coll)))

(defn prepare-defence [hand table trump] ; Estimate and apply the world changes in defend-function
  (let 
    [last-opps-hand-size @opps-hand-size]
    (do
      (swap! left-unknown difference table)
      (swap! left-unknown difference hand)
      (swap! cards-in-opps-hand difference table)
      (case @last-action
        :defend-failure ; New defence. Opp drawed
          (do
            (when (not= (count table) 1)
              (throw (Exception. "prepare-defence:defend-failure - table size != 1")))
            (swap! opps-hand-size + 
                   (min (- 6 last-opps-hand-size) @draw-size)) ; Opp drawed - update hand
            (swap! draw-size + 
                   (- last-opps-hand-size @opps-hand-size))) ; Opp drawed - update deck
        :attack-failure ; New defence. You and opp. drawed
          (do 
            (when (not= (count table) 1)
              (throw (Exception. "prepare-defence:attack-failure - table size != 1")))
            (swap! opps-hand-size + 
                   (min (- 6 last-opps-hand-size) @draw-size)) ; Opp drawed - update hand
            (swap! draw-size + 
                   (- last-opps-hand-size @opps-hand-size) ; Opp drawed - update deck
                   (- @last-hand-size (count hand)))) ; You drawed - update deck
        (:defend-success 0) ; Defence continues, so nanimo to do
          (when (and (< (count table) 3) (not= @last-action 0))
            (throw (Exception. "prepare-defence:defend-success - table size < 3"))))

      (swap! opps-hand-size dec)))) ; Opp attacked - update hand

(defn prepare-attack [hand table trump] ; Same as above, but for attack
  (let 
    [last-opps-hand-size @opps-hand-size]
    (do
      (swap! left-unknown difference table)
      (swap! cards-in-opps-hand difference table)
      (swap! left-unknown difference hand)
      (case @last-action
        :defend-failure ; Failed to defend, but opp. didn't want to attack - New attack. Opp drawed
          (do
            (when (not= (count table) 0)
              (throw (Exception. "prepare-attack:defend-failure - table size != 0")))
            (swap! opps-hand-size + 
                   (min (- 6 last-opps-hand-size) @draw-size)) ; Opp drawed - update hand
            (swap! draw-size + 
                   (- last-opps-hand-size @opps-hand-size))) ; Opp drawed - update deck
        (:attack-failure ; Failed to defeat, but opp. didn't want to attack - New attack. You and opp drawed
         :defend-success) ; New attack. Both drawed
          (do 
            (when (not= (count table) 0)
              (throw (Exception. "prepare-attack:attack-failure - table size != 0")))
            (swap! opps-hand-size + 
                   (min (- 6 last-opps-hand-size) @draw-size)) ; Opp drawed - update hand
            (swap! draw-size + 
                   (- last-opps-hand-size @opps-hand-size) ; Opp drawed - update deck
                   (- @last-hand-size (count hand)))) ; You drawed - update deck
        :attack-success ; Defeated opp and new attack or attack continues. 
          (if 
            (= (count table) 0)
            (do ; New attack, opp. failed to defend
              (swap! draw-size + 
                     (- @last-hand-size (count hand)))
              (swap! cards-in-opps-hand union @last-table)
              (swap! opps-hand-size + (count @last-table)))
            (swap! opps-hand-size dec)) ; Attack continues, opp lost 1 card
        0 ()))))

(defn split-hand [trump hand] 
  ; Splits hand into keep and pile cards - cards that we don't really want to discard
  ; (all trump cards + one most powerfull from each suit)
  ; and pile cards - that we are free to attack with
  (let
    [Keep (->> hand
            (group-by :suit)
            (mapcat (fn [[suit cards]]
                       (if (= suit trump)
                           cards
                           [(last (filter #(> (:rank %) 11) (sort-by :rank cards)))])))
            (keep identity) 
            (sort #(can-beat % %2 trump))
            (take 6)
            (set))
     Pile (clojure.set/difference (set hand) Keep)]
    (map 
      #(if (Keep %)
         {:card % :place :keep}
         {:card % :place :pile})
      hand)))

(defn defend [hand table trump] ; Main defend func
  (->> hand
    (split-hand trump)
    (filter #(can-beat (:card %) (last table) trump))
    (sort
      (fn [{acard :card aplace :place} {bcard :card bplace :place}] ; Some shamanistic comparator
       (boolean
         (if (= aplace bplace) ; If they are both in keep or both in pile
            (if (xor (= trump (:suit acard)) (= trump (:suit bcard))) ; If one of them is not a trump
                (= trump (:suit bcard))
                (if (xor (contains-equal? table acard) (contains-equal? table bcard)) ; If one of them is on table
                    (contains-equal? table acard)
                    (if (xor (contains-equal? @cards-in-opps-hand acard) (contains-equal? @cards-in-opps-hand bcard)) ; If one of them is in opps hand
                        (contains-equal? @cards-in-opps-hand bcard) 
                        (if (xor (contains-equal? @left-unknown acard) (contains-equal? @left-unknown bcard)) ; If one of them is still in unknown cards
                            (contains-equal? @left-unknown bcard)
                            (can-beat bcard acard trump))))) ; If all other match, just discard the worst
            (= aplace :pile)))))
    (first)
    (:card)))

(defn attack [hand table trump] ; Main attack func
  (let 
    [cards (if (> @draw-size 2) 
             ; If the end is near (2 cards left in drow-pile) - 
             ; all cards go into pile instead of keep
               (keep #(when (= (:place %) :pile) (:card %))
                   (split-hand trump hand))
               hand)
     cards (if (> (count table) 0)
             (filter #(contains-equal? table %) cards) ; Keep only those that are on table
             cards)]
    (->> cards
      (sort 
        (fn [acard bcard] ; another woodo-comparator
          (boolean
            (if (xor (= trump (:suit acard)) (= trump :suit bcard)) ; if one of them is trump
                (= trump (:suit bcard))
                (if (= (count-equal-non-trump cards acard trump) (count-equal-non-trump cards bcard trump)) 
                    ; Sort by the amount of same cards in hand (but for trump ones)
                    (if (= (:rank acard) (:rank bcard)) ; Rank comparison. Yeah, we really need rank here instead of can-beat
                        (if (xor (can-be-beaten acard @cards-in-opps-hand trump) (can-be-beaten bcard @cards-in-opps-hand trump)) 
                            ; if it can be beaten by one in opps-hand
                            (can-be-beaten bcard @cards-in-opps-hand trump) 
                            (if (xor (can-be-beaten bcard @left-unknown trump) (can-be-beaten acard @left-unknown trump))
                                ; if it can be beaten by one in still unknown cards
                                (can-be-beaten bcard @left-unknown trump)
                                (can-beat bcard acard trump)))
                        (< (:rank acard) (:rank bcard)))
                    (> (count-equal-non-trump cards acard trump) (count-equal-non-trump cards bcard trump)))))))
      (first))))

(defn attack-setup [{:keys [hand table trump]}]
  (do
    ; debug
    ;(println "attack-setup:hand" hand)
    ;(println "attack-setup:table" table)
    ;(println "attack-setup:trump" trump) 
    (prepare-attack (set hand) (set table) trump)
    ;(println "prepare-attack:cards-in-opps-hand" @cards-in-opps-hand)
    ;(println "prepare-attack:left-unknown" @left-unknown)
    ;(println "prepare-attack:last-action" @last-action)
    ;(println "prepare-attack:last-opps-hand-size" @last-opps-hand-size)
    ;(println "prepare-attack:last-table" @last-table)
    ;(println "prepare-attack:last-hand-size" @last-hand-size)
    ;(println "prepare-attack:opps-hand-size" @opps-hand-size)
    ;(println "prepare-attack:draw-size" @draw-size)
    ;(println "prepare-attack:split-hand" (group-by :place (split-hand trump hand)))
    ;(println "\n==========================\n")
    (let 
      [result (attack hand table trump)]
      (do
        (if result
            (reset! last-action :attack-success)
            (reset! last-action :attack-failure))
        (println "prepare-attack:result" result)
        (reset! last-table (set table))
        (reset! last-opps-hand-size @opps-hand-size)
        (reset! last-hand-size (count hand))
        (when result (swap! last-hand-size dec))
        (when result (swap! last-table union #{result}))
        result))))

(defn defence-setup [{:keys [hand table trump]}]
  (do
    ; debug
    ;(println "defence-setup:hand" hand)
    ;(println "defence-setup:table" table)
    ;(println "defence-setup:trump" trump) 
    (prepare-defence (set hand) (set table) trump) 
    ;(println "prepare-attack:cards-in-opps-hand" @cards-in-opps-hand)
    ;(println "prepare-attack:left-unknown" @left-unknown)
    ;(println "prepare-attack:last-action" @last-action)
    ;(println "prepare-attack:last-opps-hand-size" @last-opps-hand-size)
    ;(println "prepare-attack:last-table" @last-table)
    ;(println "prepare-attack:last-hand-size" @last-hand-size)
    ;(println "prepare-attack:opps-hand-size" @opps-hand-size)
    ;(println "prepare-attack:draw-size" @draw-size)
    ;(println "prepare-attack:split-hand" (group-by :place (split-hand trump hand)))
    ;(println "\n==========================\n")
    (let 
      [result (defend hand table trump)]
      (do
        (if result
            (reset! last-action :defend-success)
            (reset! last-action :defend-failure))
        (println "prepare-attack:result" result)
        (reset! last-table (set table))
        (reset! last-opps-hand-size @opps-hand-size)
        (reset! last-hand-size (count hand))
        (when result (swap! last-hand-size dec))
        (when result (swap! last-table union #{result}))
        result))))

(def bot {:attack attack-setup :defend defence-setup})

(run-game bot)

;;; Card is a map with 2 keys: rank and suit.
;;; Rank is a number from 6 to 14.
;;; 11 - Jack
;;; 12 - Queen
;;; 13 - King
;;; 14 - Ace
;;; Suit is one of the 4 keywords: :spades :hearts :diamonds :clubs
;;; Examples: {:rank 6, :suit :clubs}  {:rank 14, :suit :hearts}

;;; Functions input:
;;; Each function takes single argument - map of following structure (example):
;;; {:hand  [{:rank 6, :suit :clubs}
;;;          {:rank 8, :suit :hearts}],
;;;  :table [{:rank 6, :suite :hearts}],
;;;  :trump :clubs}
;;; Hand is a sequence of cards in your hand.
;;; Table is a sequence of cards already on table.
;;; If you are an attacker and it's start of an attack then table will be empty.
;;; If your are a defender then table will be non-empty and your need to beat last card from the table.
;;; Cards on the table can be represented like: attack  - defense - attack - defense - attack. Odd cards are attack, even cards are defense.
;;; trump is trump suit of the game.

;;; To test your solution call (run-game YOUR_SOLUTION)
;;; Your bot is the player in the lower part of screen (cards a visible to your).
;;; To run next action press SPACE, to restart game press R.
;;; When game is over (one of the players has no cards in his hand) nothing will happen when your press SPACE.
;;; If your press SPACE and nothing happen and game is not over yet, look at stacktraces, probably your bot (or built-in bot) tries to perform invalid atttack or defense.



;;; Implement program that takes 2 bots, runs game and return winner.
;;; Study init-game and next-action function in the src/durak/logic.clj. They can be used to run game.



;;; Implement bot that memorizes all cards played in game and use some smarter logic based on probability of opponents cards.
;;; Use clojure's atoms or refs to keep data between moves.
;;; If you've implemented this bot in the first task than you can skip it :)



;;; Implement attack and defend functions that they ask user input. So human can play.
;;; I don't know how to do it, may be some hacks with swing like creating and invoking dialog to ask for user input.



;;; Modify program such that it can play with 3 or 4 players.
