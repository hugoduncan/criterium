^:kindly/hide-code
(ns criterium.index)

;; ## About

;; [Criterium](https://github.com/hugoduncan/criterium) is a Clojure
;; benchmarking tool.
;;
;; **Source:** [![(GitHub repo)](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/hugoduncan/criterium)
;;
;; **Artifact:** [![Clojars Project](https://img.shields.io/clojars/v/criterium/criterium.svg)](https://clojars.org/criterium/criterium)
;;
;; **Status:** The project has a stable 0.4 branch and an alpha 0.5 branch.
;;

;; ## Notebooks
;;
;; - [Basic Usage](./criterium.basic_usage_notebook.html) - Introduction to benchmarking with criterium
;; - [Bench Options](./criterium.bench_options_notebook.html) - Predefined bench plans and viewer options
;; - [Argument Generation](./criterium.arg_gen_notebook.html) - Benchmarking with test.check generated inputs
;; - [Sampled Functions](./criterium.sampled_fn_notebook.html) - Memory-efficient function sampling with t-digest aggregation
;; - [Instrumented Functions](./criterium.instrument_fn_notebook.html) - Instrument functions for continuous performance sampling
;; - [Allocation Tracking](./criterium.allocation_tracking_notebook.html) - Memory allocation analysis with the native agent
;; - [Domain Analysis](./criterium.analyse_domain_notebook.html) - Manual domain construction and analysis
;; - [Domain Builder](./criterium.domain_builder_notebook.html) - Automated domain construction and workflows
