;;;; Listens to port 4321

(defparameter my-socket (socket-server 4321)) ; defines the socket

(defparameter my-stream (socket-accept my-socket)) ; listens
