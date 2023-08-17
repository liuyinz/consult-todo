# consult-todo

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA](http://melpa.org/packages/consult-todo-badge.svg)](http://melpa.org/#/consult-todo)

Search and jump hl-todo keywords in buffer and project with consult.

<!-- markdown-toc start -->

## Contents

- [consult-todo](#consult-todo)
  - [Install](#install)
    - [dependencies](#dependencies)
    - [package](#package)
  - [Usage](#usage)
  - [Customization](#customization)
  - [Todo](#todo)
  - [License](#license)

<!-- markdown-toc end -->

## Install

### dependencies

- Emacs, version >= 26.3
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

## Customization

- `consult-todo-narrow-alist`: Mapping of narrows and keywords, if it's nil, use default value below instead.

```emacs-lisp
(defconst consult-todo--narrow
  '((?t . "TODO")
    (?f . "FIXME")
    (?b . "BUG")
    (?h . "HACK"))
  "Default mapping of narrow and keywords.")
```

- `consult-todo-narrow-other`: Narrow for other keywords which are not included in `consult-todo-narrow-alist`, use `.` by default.

## Todo

- [ ] Implement project searching for TODO keywords

## License

See [LICENSE](LICENSE).

[melpa]: http://melpa.org/#/binky
