# consult-todo

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA](http://melpa.org/packages/consult-todo-badge.svg)](http://melpa.org/#/consult-todo)

Search and jump hl-todo keywords in buffers with consult.

<!-- markdown-toc start -->

## Contents

- [consult-todo](#consult-todo)
  - [Install](#install)
    - [dependencies](#dependencies)
    - [package](#package)
  - [Usage](#usage)
  - [Customization](#customization)
  - [License](#license)

<!-- markdown-toc end -->

## Install

### dependencies

- Emacs, version >= 27.1
- [hl-todo](https://github.com/tarsius/hl-todo)
- [consult](https://github.com/minad/consult)

### package

- Manually

Clone and add to `load-path`, require the package.

- Melpa

This package is available on [MELPA].
Install with `M-x package-install` `RET` `consult-todo` within Emacs.

## Usage

```elisp
;; Directly
(require 'consult-todo)

;; Or with use-package
(use-package consult-todo :demand t)
```

- `consult-todo`: Jump to hl-todo keywords in current buffer.

- `consult-todo-all`: Jump to hl-todo keywords in all live buffers.

## Customization

- `consult-todo-narrow`: Mapping of narrows and keywords, if it's nil, use default value below instead.

```emacs-lisp
(defconst consult-todo--narrow
  '((?t . "TODO")
    (?f . "FIXME")
    (?b . "BUG")
    (?h . "HACK"))
  "Default mapping of narrow and keywords.")
```

- `consult-todo-other`: Cons for other missing keywords, `(?. . "OTHER")` by default.

## License

See [LICENSE](LICENSE).

[melpa]: http://melpa.org/#/consult-todo
