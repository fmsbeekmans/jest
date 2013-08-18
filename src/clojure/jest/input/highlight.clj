(ns jest.input.highlight)

(let [highlighted-cells (atom {})]
  (defn highlight-cell
    [pointer-id coords]
    (swap! highlighted-cells
           #(assoc % pointer-id
                   (conj (vec (get % pointer-id)) coords))))
  (defn remove-highlighted-cells [pointer-id]
    (swap! highlighted-cells #(dissoc % pointer-id)))
  (defn get-highlighted-cells []
    @highlighted-cells))
