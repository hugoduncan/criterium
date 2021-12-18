(require 'ox-publish)

(setq org-export-html-postamble nil)

(setq self-file-name (or buffer-file-name
                         load-file-name)
      self-parent-name (or (file-name-directory self-file-name) "./"))

(setq org-publish-project-alist
      `(("docs"
         :base-directory ,(or (file-name-directory self-file-name) "doc-src")
         :publishing-directory ,(concat self-parent-name "../doc/")
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :with-creator nil
         :org-export-html-postamble nil
         )
        ("css"
         :base-directory ,(concat self-parent-name "../site-resources/css/")

         :base-extension "css"
         :publishing-directory ,(concat self-parent-name "../doc/css/")

         :publishing-function org-publish-attachment
         :recursive t)
        ("all" :components ("docs" "css"))))

;; (org-publish-all)
