
(set-frame-font "Hack 13" nil t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      ))

(provide 'interface)

