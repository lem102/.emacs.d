(require 'treesit)

(setq treesit-language-source-alist
      '((c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" "master" "src")))

(setq treesit-load-name-override-list '((c-sharp "libtree-sitter-csharp" "tree_sitter_c_sharp")))

(setq major-mode-remap-alist '((csharp-mode . csharp-ts-mode)))

