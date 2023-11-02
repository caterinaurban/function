module TerminationBoxes =
  TerminationIterator.TerminationIterator (DecisionTree.TSAB)
module TerminationBoxesOrdinals =
  TerminationIterator.TerminationIterator (DecisionTree.TSOB)
module TerminationOctagons =
  TerminationIterator.TerminationIterator (DecisionTree.TSAO)
module TerminationOctagonsOrdinals =
  TerminationIterator.TerminationIterator (DecisionTree.TSOO)
module TerminationPolyhedra =
  TerminationIterator.TerminationIterator (DecisionTree.TSAP)
module TerminationPolyhedraOrdinals =
  TerminationIterator.TerminationIterator (DecisionTree.TSOP)
module GuaranteeBoxes =
  GuaranteeIterator.GuaranteeIterator (DecisionTree.TSAB)
module GuaranteeBoxesOrdinals =
  GuaranteeIterator.GuaranteeIterator (DecisionTree.TSOB)
module GuaranteeOctagons =
  GuaranteeIterator.GuaranteeIterator (DecisionTree.TSAO)
module GuaranteeOctagonsOrdinals =
  GuaranteeIterator.GuaranteeIterator (DecisionTree.TSOO)
module GuaranteePolyhedra =
  GuaranteeIterator.GuaranteeIterator (DecisionTree.TSAP)
module GuaranteePolyhedraOrdinals =
  GuaranteeIterator.GuaranteeIterator (DecisionTree.TSOP)
module RecurrenceBoxes =
  RecurrenceIterator.RecurrenceIterator (DecisionTree.TSAB)
module RecurrenceBoxesOrdinals =
  RecurrenceIterator.RecurrenceIterator (DecisionTree.TSOB)
module RecurrenceOctagons =
  RecurrenceIterator.RecurrenceIterator (DecisionTree.TSAO)
module RecurrenceOctagonsOrdinals =
  RecurrenceIterator.RecurrenceIterator (DecisionTree.TSOO)
module RecurrencePolyhedra =
  RecurrenceIterator.RecurrenceIterator (DecisionTree.TSAP)
module RecurrencePolyhedraOrdinals =
  RecurrenceIterator.RecurrenceIterator (DecisionTree.TSOP)
module CTLBoxes = CTLIterator.CTLIterator (DecisionTree.TSAB)
module CTLBoxesOrdinals = CTLIterator.CTLIterator (DecisionTree.TSOB)
module CTLOctagons = CTLIterator.CTLIterator (DecisionTree.TSAO)
module CTLOctagonsOrdinals = CTLIterator.CTLIterator (DecisionTree.TSOO)
module CTLPolyhedra = CTLIterator.CTLIterator (DecisionTree.TSAP)
module CTLPolyhedraOrdinals = CTLIterator.CTLIterator (DecisionTree.TSOP)
module CFGCTLBoxes = CFGCTLIterator.CFGCTLIterator (DecisionTree.TSAB)
module CFGCTLBoxesOrdinals = CFGCTLIterator.CFGCTLIterator (DecisionTree.TSOB)
module CFGCTLOctagons = CFGCTLIterator.CFGCTLIterator (DecisionTree.TSAO)
module CFGCTLOctagonsOrdinals =
  CFGCTLIterator.CFGCTLIterator (DecisionTree.TSOO)
module CFGCTLPolyhedra = CFGCTLIterator.CFGCTLIterator (DecisionTree.TSAP)
module CFGCTLPolyhedraOrdinals =
  CFGCTLIterator.CFGCTLIterator (DecisionTree.TSOP)