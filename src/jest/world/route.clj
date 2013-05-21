(ns jest.world.route
  "Functions for adding, removing and searching for routes along roads, rails and canals."
  (:use jest.util
        jest.world.path
        jest.world.cell))

(defn- add-route
  "Adds a route for resources of the specified color to an
  existing path"
  [path color]
  (assoc path :routes
         (set (conj (path :routes) color))))

(defn- remove-route
  "Removes any existing routes for the specified color from an
  existing path"
  [path color]
  {:pre [(contains? color (path :routes))]}
  (update-in path [:routes]
             #(disj % color)))

(defn preferred-path
  "Returns the preferred outgoing path for the specified cell, type and color"
  [c type color]
  )

(defn build-route
  "Adds a colored route to an existing path in cell c"
  [c dir color]
  (dosync
   (alter-cell c #(add-route % color) (get-in c [:paths dir]))))

(defn remove-route
  "removes a colored route from an existing path in cell c"
  [c dir color]
  (dosync
   (alter-cell c #(remove-route % color) (get-in c [:paths dir]))))
