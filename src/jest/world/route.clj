(ns jest.world.route
  "Functions for adding, removing and searching for routes along roads, rails and canals."
  (:use jest.util
        jest.world.path
        jest.world.cell))

(defn- add-route
  "Adds a route for resources of the specified color to an
  existing path"
  [path color]
  {:pre [path
         color
         (not (contains? (:routes path) color))]}
  (assoc path :routes
         (set (conj (:routes path) color))))

(defn- remove-route
  "Removes any existing routes for the specified color from an
  existing path"
  [path color]
  {:pre [(contains? color (:routes path))]}
  (update-in path [:routes]
             #(disj % color)))

(defn build-route
  "Adds a colored route to an existing path in cell c"
  [c dir color]
  (dosync
   (alter-cell c #(add-route % color) (get-in c [:paths dir]))))

(defn unbuild-route
  "unbuilds a colored route from an existing path in cell c"
  [c dir color]
  (dosync
   (alter-cell c #(remove-route % color) (get-in c [:paths dir]))))
