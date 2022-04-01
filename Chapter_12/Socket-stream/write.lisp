;;;; Writes to socket on port 4321

(defparameter my-stream (socket-connect 4321 "127.0.0.1"))
