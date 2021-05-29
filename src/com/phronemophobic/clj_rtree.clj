(ns com.phronemophobic.clj-rtree
  (:import com.github.davidmoten.rtree.RTree
           com.github.davidmoten.rtree.Entries
           com.github.davidmoten.rtree.geometry.Geometries
           ))


(defn- jtree [rects]
  (RTree/create (map (fn [{:keys [x y w h] :as rect}]
                       (let [geom (Geometries/rectangle
                                   (double x)
                                   (double y)
                                   (double (+ x w))
                                   (double (+ y h)))]
                         (Entries/entry rect geom)))
                     rects)))

(defn- jsearch [rt [x y]]
 (-> (.search ^RTree rt (Geometries/point (double x) (double y)))
     (.toBlocking)
     (.toIterable)
     (->> (map (fn [entry]
                 (.value ^com.github.davidmoten.rtree.Entry entry))))))


(defn- jadd [rt {:keys [x y w h] :as rect}]
  (let [geom (Geometries/rectangle
              (double x)
              (double y)
              (double (+ x w))
              (double (+ y h)))]
    (.add ^RTree rt rect geom)))


(defn rtree
  ([]
   (-> (RTree/star)
       (.maxChildren 4)
       (.create) ))
  ([rects]
   (jtree rects)))

(defn search [rt [x y :as pt]]
  (jsearch rt pt))

(defn add [rt {:keys [x y w h] :as rect}]
  (jadd rt rect))

