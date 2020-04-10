# emacs.d

Repozytorium z moją konfiguracją Emacs'a 

## Instalacja

1. Usuń pozostałe foldery konfiguracyjne Emacs'a
2. Sklonuj repozytorium do folderu `~/.emacs.d/`
```
cd ~/.emacs.d/
git clone git@bitbucket.org:admq/emacs.d.git
```
3. Instalacja pakietów wykona się automatycznie za pomocą `use-package`, które nie jest domyślnie zawarte w instalacji, stąd błędy o braku pakietu. Aby zainstalować `use-package`, uruchom Emacs i zainstaluj go z MELPA
```
M-x package-install RET use-package
```
4. Uruchom ponownie Emacs w celu poprawnej inicjalizacji.

## TODO

1. Zaawansowane wsparcie LaTeX
2. Rozwinięcie konfiguracji `org-mode`
3. Wszystko inne
