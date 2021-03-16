;;; tools/wrench/config.el -*- lexical-binding: t; -*-

(when (featurep! +spotify)
  (load! "+spotify"))

(when (featurep! +container)
  (load! "+container"))
