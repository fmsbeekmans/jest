(ns jest.core
  (:import (com.mlawrie.yajtl TUIOEvent TUIOCursor TUIOReceiver)))

(def tuio (TUIOReceiver. 800 600))

(def handler (proxy [TUIOEvent] []
			  (moveCursorEvent [cursor]
					   (def move-event "Move cursor: " cursor))
			  (newCursorEvent [cursor]
					  (def new-event "New cursor: " cursor))
			  (removeCursorEvent [cursor]
					     (def remove-event "Remove cursor: " cursor))))

(.setHandler tuio handler)
