(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

;; Command functions

(defn look
  "Получить описание окружающих окрестностей и его содержимого."
  []
  (str (:desc @player/*current-room*)
       "\nВыходы: " (keys @(:exits @player/*current-room*)) "\n"
       (str/join "\n" (map #(str "Здесь есть: " % ".\n")
                           @(:items @player/*current-room*)))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if target
       (do
         (move-between-refs player/*name*
                            (:inhabitants @player/*current-room*)
                            (:inhabitants target))
         (set! player/*hp* (+ player/*hp* (:hp-change target)))
         (when (> player/*hp* 100)
           (set! player/*hp* 100))
         (when (<= player/*hp* 0)
           ;(println "loser")
           (set! player/*status* "loser")
           )
         (when (= target-name :win_room)
           ;(println "winner")
           (set! player/*status* "winner")
           )
         (ref-set player/*current-room* target)
         (look))
       "Ты не можешь пойти в этом направлении."))))

(defn grab
  "Поднять что то."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "Ты залутал  " thing "."))
     (str "Здесь нет предмета " thing "."))))

(defn discard
  "Скинуть предмет."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "Ты скинул " thing "."))
     (str "У тебя нет предмета " thing "."))))

(defn inventory
  "Посмтотреть инвентарь."
  []
  (str "Твой инвентарь:\n"
       (str/join "\n" (seq @player/*inventory*))))

(defn hp
  "Посмотреть количество хэпэшек."
  []
  (str "У тебя осталось " player/*hp* " хэпэшек."))

(defn detect
  "Если у вас есть детектор, вы можете увидеть, в какой комнате находится предмет."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str item " находится в комнате " (:name room))
      (str item " такого предмета нигде нет."))
    "Нельзя читерить."))

(defn say
  "Скажите что-нибудь вслух, чтобы все в комнате могли услышать."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println message)
        (println player/prompt)))
    (str "Ты бромолчал: " message)))

(defn help
  "Показать доступные команды и то, что они делают."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

;; Command data

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "help" help
               "hp" hp})

;; Command handling.

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "Неопознанная команда. Для просмотра списка команд введи help")))
