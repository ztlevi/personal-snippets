
  (defface mode-line-linum-number-face
    '((t (:inherit font-lock-keyword-face)))
    "mode-line linum number face")
  (set-face-attribute 'mode-line-linum-number-face nil
                      :box `(:line-width 5 :color ,(face-attribute 'mode-line :background)))
