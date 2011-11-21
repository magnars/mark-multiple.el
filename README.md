mark-multiple.el
================

An emacs extension that sorta lets you mark several regions at once.

More precisely, it allows for one master region, with several mirror
regions. The mirrors are updated inline while you type. This allows for some
awesome functionality. Or at least, some more visually pleasing insert and
replace operations.

Done
----
* A general library for managing master and mirrors
* `mark-more-like-this` which selects next/previous substring in the buffer that
  matches the current region.
* `inline-string-rectangle` which works like `string-rectangle` but lets you
  write inline - making it less error prone.

Installation
------------

    git submodule add https://github.com/magnars/mark-multiple.el.git site-lisp/mark-multiple

Then add the modules you want to your init-file:

    (require 'inline-string-rectangle)
    (global-set-key (kbd "C-x r t") 'inline-string-rectangle)

    (require 'mark-more-like-this)
    (global-set-key (kbd "") 'mark-previous-like-this)
    (global-set-key (kbd "") 'mark-next-like-this)
    (global-set-key (kbd "") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)

I'm sure you'll come up with your own keybindings.

Ideas for more
--------------
* `rename-html-tag` which updates the matching tag while typing.
* `js-rename-local-var` which renames the variable at point in the local file.

Bugs and gotchas
----------------
* Adding a master and mirrors does not remove the active region. This might feel
  strange, but turns out to be practical.

* The current mark-multiple general library lets you do stupid shit, like adding
  overlapping mirrors. That's only a problem for people who want to write their
  own functions using `mm/create-master` and `mm/add-mirror`.

A wild idea
-----------

Is this a subset of a crazy multiple-point module? How would that even work?

There is one use for it I can see, which is editing the end of lines. Set up one
cursor at the end of each line, then just edit normally. The command is repeated
for each position.

Might be too far out there. I still want to do edit-end-of-lines tho.
