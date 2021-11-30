(ns caesarhu.project-euler.euler-189-1
  (:import com.google.ortools.Loader
           [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar CpSolverSolutionCallback]))

(Loader/loadNativeLibraries)

(defn cp-solve
  []
  (let [model (CpModel.)
        numVals 3
        x (.newIntVar model 0 (dec numVals) "x")
        y (.newIntVar model 0 (dec numVals) "y")
        z (.newIntVar model 0 (dec numVals) "z")
        _ (.addDifferent model x y)
        solver (CpSolver.)
        status (.solve solver model)]
    (if (or (= status CpSolverStatus/OPTIMAL) (= status CpSolverStatus/FEASIBLE))
      [(.value solver x) (.value solver y) (.value solver z)]
      :no-solution)))

(defn count-solutions
  []
  (let [counter (atom 0)
        cb (proxy [CpSolverSolutionCallback] []
             (onSolutionCallback []
               (swap! counter inc)))
        model (CpModel.)
        numVals 3
        x (.newIntVar model 0 (dec numVals) "x")
        y (.newIntVar model 0 (dec numVals) "y")
        z (.newIntVar model 0 (dec numVals) "z")
        _ (.addDifferent model x y)
        solver (CpSolver.)]
    (.setEnumerateAllSolutions (.getParameters solver) true)
    (.solve solver model cb)
    @counter))