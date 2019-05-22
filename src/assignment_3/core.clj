(ns assignment-3.core
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File]))

  ; Constant data
  (def kirch-rotations [0 1 2 3 4 5 6 7])
  (def final-bin-size 8)
  (def kirch-filters-vec [[[-1 0 1][-2 0 2][-1 0 1]]
                          [[-2 -1 0][-1 0 1][0 1 2]]
                          [[-1 -2 -1][0 0 0][1 2 1]]
                          [[0 -1 -2][1 0 -1][2 1 0]]
                          [[1 0 -1][2 0 -2][1 0 -1]]
                          [[2 1 0][1 0 -1][0 -1 -2]]
                          [[1 2 1][0 0 0][-1 -2 -1]]
                          [[0 1 2][-1 0 1][-2 -1 0]]])

  (def filename-types ["car" "plane" "train"])
  (def filename-counts (range 1 21))

  ; -------------------------

  (defn new-image
    [w h]
    (BufferedImage. w h BufferedImage/TYPE_INT_RGB))

  (defn save-image
    [image extension filename]
    (let [file (File. filename)]
      (ImageIO/write image extension file)))

  (defn load-image
    [filename]
    (let [file (File. filename)]
      (ImageIO/read file)))

  (defn get-width
    [image]
    (.getWidth image))

  (defn get-height
    [image]
    (.getHeight image))

  (defn get-rgb
    "Returns a vector of RGB values of the selected pixel"
    [image x y]
    (let [rgb (.getRGB image x y)
          r (bit-shift-right (bit-and rgb 0xFF0000) 16)
          b (bit-shift-right (bit-and rgb 0xFF00) 8)
          g (bit-and rgb 0xFF)]
      (vec (list r g b))))

  (defn set-pixel
    "Returns the new BufferedImage with the updated pixel"
    [image x y [r g b]]
    (let [rgb (+ (bit-shift-left r 16)
                 (bit-shift-left b 16)
                 (bit-shift-left g 0))]
      (.setRGB image x y rgb)))

  (defn set-rgb
    "Function to set the RGB components of a pixel."
    [image x y [red green blue]]
   (let [rgb (+ (bit-shift-left red 16)
                (bit-shift-left blue 8)
                (bit-shift-left green 0) )]
       (.setRGB image x y rgb)))

    (defn set-grey
        "Function to set the grey value of a pixel."
        [image x y grey]
        (set-rgb image x y [grey grey grey]))

    (defn get-line
      "Returns the line of pixels as vector of r-values [r1 r2 ...]"
      ([image row]
        (get-line image 0 row []))

      ([image curr-col row im-list]
        (let [col-limit (get-width image)]
          (if (= curr-col col-limit)
            im-list
            (recur image (inc curr-col) row (vec (concat im-list (vector (first (get-rgb image curr-col row))))))))))

    (defn get-list-of-image
      "Returns a vector of the image as a vector of vectors of r-values [[r1 r2 ...] [r12 r13 ...]]"
      ([image]
        (get-list-of-image image 0 []))

      ([image curr-row im-list]
        (let [row-limit (get-height image)]
          (if (= curr-row row-limit)
            im-list
            (recur image (inc curr-row) (vec (concat im-list (get-line image curr-row))))))))

    (defn convert-to-greyscale
        "Returns the new BufferedImage in grayscale"
        [image w-red w-green w-blue]
        (let [height (get-height image)
              width (get-width image)]
            (dotimes [x width]
                (dotimes [y height]
                    (let [rgb (get-rgb image x y)
                          final-red (* (first rgb) w-red)
                          final-green (* (second rgb) w-green)
                          final-blue (* (last rgb) w-blue)
                          final-grey (int (reduce + (list final-red final-green final-blue)))]
                        (set-grey image x y final-grey))))
            image))

    (defn create-side-by-side
        ""
        [final-image image-1 image-2]
        (let [width (get-width final-image)
              height (get-height final-image)]
        (dotimes [x width]
            (dotimes [y height]
                (if (< x (int (/ width 2)))
                    (set-rgb final-image x y (get-rgb image-1 x y))
                    (set-rgb final-image x y (get-rgb image-2 x y))
                )
            )
        )
        final-image))

    (defn get-copy
        "Returns a copy of the passed image"
        [original]
        (let [width (get-width original)
              height (get-height original)
              copy-img (new-image width height)]
            (dotimes [x width]
                (dotimes [y height]
                    (set-rgb copy-img x y (get-rgb original x y))))
            copy-img))

  (defn bin-pixel-list
    "Returns a vector of binned pixels"
    [flat-list mult-value]
    (map #(int (* %1 mult-value)) flat-list))

  (defn fill-empty-hist-bins
    ([histogram bin-size]
      (fill-empty-hist-bins histogram 0 bin-size))

    ([histogram curr-bin bin-size]
      (if (= curr-bin bin-size)
          histogram
          (if (= (get histogram curr-bin) nil)
              (recur (assoc histogram curr-bin 0) (inc curr-bin) bin-size)
              (recur histogram (inc curr-bin) bin-size)))))

  (defn create-histogram-from-image
    "Returns the histogram of the provided image"
    ([image bin-size]
      (let [flat-list (get-list-of-image image)
          distinct-pixels 256
          bins bin-size
          histogram (frequencies (bin-pixel-list flat-list (/ bin-size distinct-pixels)))]
            (fill-empty-hist-bins histogram bin-size)))

    ([image bin-size restrictor-pixels]
      (let [flat-list (get-list-of-image image)
          distinct-pixels restrictor-pixels
          bins bin-size
          histogram (frequencies (bin-pixel-list flat-list (/ bin-size distinct-pixels)))]
            (fill-empty-hist-bins histogram bin-size))))

  (defn threshold-image
    "Returns a image that has been threshold(ed)"
    [image threshold-value]
    (let [width (get-width image)
          height (get-height image)]
          (dotimes [x width]
              (dotimes [y height]
                  (let [this-pixel-value (first (get-rgb image x y))]
                      (if (< this-pixel-value threshold-value)
                        (set-grey image x y 0)
                        (set-grey image x y 255)))))
                  image))

  (defn get-pos-in-3x3
    "Returns the value in the 3x3 matrix"
    [matrix x y]
    (get (get matrix x) y))

  ; filter looks like this -> [[0.1 0.1 0.1][0.1 0.1 0.1][0.1 0.1 0.1]]
  ;    or [[0.25 0.5 0.25][-.5 -3 0.5][0.25 0.5 0.25]]
  (defn calc-pixel-w-filter
    "Returns a pixel value after the application of the filter to the location"
      [image x y filter-matrix]
        ; need to deal with negative values so do clamping here
        (let [filtered-value (+ (int (apply + (map #(* (first (get-rgb image (+ x %1) (+ y %2))) (get-pos-in-3x3 filter-matrix %3 %4)) [1 0 -1 1 0 -1 1 0 -1] [1 1 1 0 0 0 -1 -1 -1] [0 1 2 0 1 2 0 1 2] [0 0 0 1 1 1 2 2 2]))) 127)]
          (cond
            (< filtered-value 0) 0
            (> filtered-value 255) 255
            :else filtered-value)))

  ; This goes through each pixel from the orginal - calls calc-pix.. and saves that to the new image
  (defn filter-image
    "Returns a filtered image"
    [image image-width image-height filter-matrix]
    (let [filtered-image (new-image image-width image-height)]
          (dotimes [x (- image-width 2)]
              (dotimes [y (- image-height 2)]
                  (set-grey filtered-image (+ x 1) (+ y 1) (calc-pixel-w-filter image (+ x 1) (+ y 1) filter-matrix))))
          filtered-image))

  (defn kirch
    [filename rotation-index]
    "Returns a BufferedImage containing the reuslts of the application of the kirsch filter"
    (let [loaded-image (load-image filename)
          width (get-width loaded-image)
          height (get-height loaded-image)
          blurred-image (filter-image (filter-image (filter-image loaded-image width height [[0.1 0.1 0.1][0.1 0.1 0.1][0.1 0.1 0.1]]) width height [[0.1 0.1 0.1][0.1 0.1 0.1][0.1 0.1 0.1]]) width height [[0.1 0.1 0.1][0.1 0.1 0.1][0.1 0.1 0.1]])]
        (filter-image blurred-image width height (get kirch-filters-vec rotation-index))))

  (defn edge-magnitude-hist
    [filename]
    "Returns a normalised frequency histogram with 8 bins containing edge magnitudes"
    (let [kirch-image-list (map #(kirch filename %) kirch-rotations) ;1 2 3 4 5 6 7
          width (get-width (first kirch-image-list))
          height (get-height (first kirch-image-list))
          final-kirch-image (new-image width height)]
      (dotimes [x width]
          (dotimes [y height]
            (let [list-of-grey-values (map #(first (get-rgb %1 x y)) kirch-image-list)]
              (set-grey final-kirch-image x y (apply max list-of-grey-values)))))
        ;(fill-empty-hist-bins (create-histogram-from-image final-kirch-image final-bin-size) final-bin-size)))
        final-kirch-image))

  (defn edge-direction-hist
    [filename]
    "Returns a normalised direction histogram with 8 bins containing edge directions"
      (let [kirch-image-list (map #(kirch filename %) kirch-rotations) ;1 2 3 4 5 6 7
            width (get-width (first kirch-image-list))
            height (get-height (first kirch-image-list))
            final-kirch-direc-image (new-image width height)]
        (dotimes [x width]
            (dotimes [y height]
              (let [list-of-grey-values (map #(first (get-rgb %1 x y)) kirch-image-list)
                    max-value (apply max list-of-grey-values)]
                (set-grey final-kirch-direc-image x y (.indexOf list-of-grey-values max-value))))) ;<- need to find the index of the max value since that will be our direction value
          (fill-empty-hist-bins (create-histogram-from-image final-kirch-direc-image final-bin-size 8) final-bin-size)))
          ;final-kirch-direc-image))

  (defn normalise-histogram
    [histogram]
    "Returns a normalised histogram [0-1]" ;Sort them first
    (let [sum-of-all-bins (apply + (vals histogram))]
      (map #(float (/ (second %) sum-of-all-bins)) (sort (seq histogram)))))

  (defn normalise-image-histogram
    [filename] ;Sort them first
    "Returns a histogram that has been normalised from an image"
    (let [loaded-image (load-image filename)
          histogram (fill-empty-hist-bins (create-histogram-from-image loaded-image final-bin-size) final-bin-size)]
        (normalise-histogram histogram)))

  (defn normalise-descriptor
    "Returns the values of the descriptor to be between 0 and 1 overall"
    [descriptor-list]
    (let [d-bin-size (* final-bin-size 3)
          sum-of-all-bins (apply + descriptor-list)]
      (map #(float (/ % sum-of-all-bins)) descriptor-list)))

  (defn image-descriptor
    [filename]
    "Returns a length 24 vector (8 + 8 + 8) descriptor for the given image"
    (let [edge-histogram (normalise-histogram (edge-magnitude-hist filename))
          direction-histogram (normalise-histogram (edge-direction-hist filename))
          normalised-histogram (normalise-image-histogram filename)]
        (normalise-descriptor (flatten (list edge-histogram direction-histogram normalised-histogram)))))

  (defn image-similarity
    [filename1 filename2]
    "Returns a value between 0 and 1 which represents how similar the two images are"
    (let [image1-description (image-descriptor filename1)
          image2-description (image-descriptor filename2)]
        (do (println (str "Results for image-similarity of " filename1 " & " filename2 " equals:"))
            (apply + (map #(min %1 %2) image1-description image2-description)))))

  (defn -main
      ""
      [& args]
      (let [load-path "./resources/vehicle_images/"
            save-path "./resources/final-images/"
            save-type "jpg"]
          (map #(image-similarity (str load-path %1 ".jpg") (str load-path %2 ".jpg")) ["car1" "car1" "car1"] ["car1" "car2" "plane1"])))
