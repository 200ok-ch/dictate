#!/usr/bin/env bb

(ns dictate
  (:require [shell-smith.core :as smith]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [babashka.process :as p]
            [babashka.fs :as fs]
            [cheshire.core :as json]))

(def usage "
dictate - Background speech-to-text dictation tool

Usage:
  dictate --service [options]
  dictate --toggle
  dictate -h | --help

Options:
  -a --device=<device>      Device to record from [default: default]
  -d --delay=<delay>        Type delay in millisecs [default: 25]
  -h --help                 Show this help message
  -m --model=<model>        Model [default: whisper-1]
  -p --api-path=<api-path>  API path [default: /v1/audio/transcriptions]
  -r --api-root=<api-root>  API root [default: https://api.openai.com]
  -s --service              Run as background service process
  -t --toggle               Toggle the recording mode (active/inactive)

Examples:
  dictate --service                   Start service with default device
  dictate --service --device=hw:1,0   Start service with specific device
  dictate --toggle                    Toggle recording on/off

See the README for more details: https://github.com/200ok-ch/dictate
")

(def indicator "🔴")

(def state-file (str (System/getProperty "user.home") "/.dictate.state"))

(defn notify-cmd [message]
  (p/shell "notify-send" "Dictate" message))

(defn write-state! [state]
  "Write the current state to the state file"
  (spit state-file (if (= state :active) indicator "")))

(defn read-state []
  "Read the current state from the state file"
  (try
    (let [content (str/trim (slurp state-file))]
      (if (= content indicator) :active :inactive))
    (catch Exception _
      :inactive)))

;; TODO: Instead of this simple approach with arecord, use SOX with
;; the silence-detecting feature to split into multiple files that can
;; be processed independently.
;;
;;   rec -q -V0 -e signed -L -c 1 -b 16 -r 44100 -t raw \
;;      - trim 0 "$DURATION" silence 1 0.1 "$MIN_VOLUME" 1 "$SILENCE_LENGTH" "$MIN_VOLUME" | \
;;      sox -t raw -r 44100 -b 16 -e signed -c 1 - "$OUTPUT_FILE"
;;
(defn record-audio-cmd [device temp-file]
  "Record audio to temporary file using arecord"
  (p/process {:continue true} "timeout" "1m" "arecord" "-D" device "-f" "cd" "-t" "wav" temp-file))

(defn transcribe-audio [{:keys [api-root api-path api-key model]} audio-file]
  "Send audio to OpenAI Whisper API for transcription"
  (try
    (when (str/blank? api-key)
      (throw (Exception. "api-key must be configured")))
    (let [result (p/shell {:out :string :continue true}
                          "curl" "-s" "-X" "POST"
                          (str api-root api-path)
                          "-H" (str "Authorization: Bearer " api-key)
                          "-H" "Content-Type: multipart/form-data"
                          "-F" (str "file=@" audio-file)
                          "-F" (str "model=" model))]
      ;; (prn result)
      ;; Parse JSON response and extract text
      (let [response (str/trim (:out result))]
        (when-not (str/blank? response)
          (let [payload (json/parse-string response true)]
            ;; (pprint/pprint payload)
            (:text payload)))))
    (catch Exception e
      (println "Error transcribing audio:" (.getMessage e) e)
      nil)))

(defn type-text [{:keys [delay]} text]
  "Type the transcribed text using xdotool"
  (when-not (str/blank? text)
    (p/shell "xdotool" "type" "--delay" (str delay) text)))

(defn recording-loop [{:keys [device] :as config}]
  "Main recording loop that runs while service is active with continuous recording"
  (println "Dictate service started. Press Ctrl+C to stop or use --toggle to control.")
  (while true
    (when (= (read-state) :active)
      (let [temp-file (str "/tmp/dictate-" (System/currentTimeMillis) ".wav")]
        (try
          (println "Recording segment...")
          ;; Use rec (sox) with silence detection
          (let [rproc (p/process {:continue true}
                                "rec" "-q" temp-file
                                "silence" "1" "0.1" "1%" "1" "2.0" "1%"
                                "trim" "0" "60")]  ; Max 1 minute per segment
            ;; Wait for the recording process to complete (silence detected or max time reached)
            (while (and (= (read-state) :active)
                        (p/alive? rproc))
              (Thread/sleep 250))
            ;; if the process is still running at this point we need to kill it
            (when (p/alive? rproc)
              (p/destroy rproc))
            (deref rproc))

          (println "Recording segment complete.")
          (when (fs/exists? temp-file)
            (println "Transcribing...")
            (when-let [text (transcribe-audio config temp-file)]
              (println "Typing:" text)
              (type-text config text)))

          (catch Exception e
            (println "Error in recording loop:" (.getMessage e) e))
          (finally
            (when (fs/exists? temp-file)
              (fs/delete temp-file))))))

    ;; Sleep briefly when inactive
    (Thread/sleep 500)))

(defn start-service! [config]
  "Start the service process"
  ;; Handle cleanup on exit
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(write-state! :inactive)))
  ;; Initialize as inactive
  (write-state! :inactive)
  ;; Start recording loop
  (recording-loop config))

(defn toggle-mode! []
  "Toggle between active and inactive recording modes"
  (let [current-state (read-state)
        new-state (if (= current-state :active) :inactive :active)]
    (write-state! new-state)
    (println (str "Dictate mode: " (name new-state)))
    ;; force update i3status for the instant red bubble
    (p/shell "killall -USR1 i3status")
    ;;(notify-cmd (str "Mode: " (name new-state) (when (#{:active} new-state) (str " " indicator))))
    ))

(defn main [args]
  (let [config (smith/config usage)]
    (cond
      (:service config)
      (start-service! config)

      (:toggle config)
      (toggle-mode!)

      :else
      (println usage))))

(when (= *file* (System/getProperty "babashka.file"))
  (main *command-line-args*))
