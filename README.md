# emacs.d

Repozytorium z moją konfiguracją Emacs'a 

![rel-xkcd](https://imgs.xkcd.com/comics/automation.png)

## Instalacja

1. Usuń pozostałe foldery konfiguracyjne Emacs'a
2. Sklonuj repozytorium do folderu `~/.emacs.d/`
3. Instalacja pakietów wykona się automatycznie za pomocą `use-package`, które nie jest domyślnie zawarte w instalacji, stąd błędy o braku pakietu. Aby zainstalować `use-package`, uruchom Emacs i zainstaluj go z MELPA

			M-x package-install RET use-package

4. Uruchom ponownie Emacs w celu poprawnej inicjalizacji.

### Dodatkowe kroki: konfiguracja daemon mode

Uruchamianie instancji Emacs'a w trybie daemon ma wiele zalet, które widzę. Jak będzie mi się chciało to je wyjaśnię.

5. Zdefiniuj uruchomienie daemon przy starcie systemu przechodząc w ścieżce eksploratora plików Windows do `shell:staratup` i dodając skrót do ścieżki `"C:\sciezka-do-emacs\bin\runemacs.exe" --daemon`.

6. Zdefiniuj skrót do nowego okna klienta jako skrót do ścieżki `"C:\sciezka-do-emacs\bin\emacsclientw.exe" -c -n -a runemacs.exe`.

7. Dodaj do rejestru elementy menu umożliwiające otwieranie plików za pomocą istniejących bądź nowych okien Emacs'a za pomocą skryptu `.reg`:
		
		Windows Registry Editor Version 5.00
		;; Be sure to set the correct path to Emacs on your system!
		[HKEY_CURRENT_USER\Software\Classes\*\shell]


		;; Open file in existing frame
		[HKEY_CURRENT_USER\Software\Classes\*\shell\emacsopencurrentframe]
		@="&Emacs: Edit in existing window"
		"icon"="C:\\sciezka-do-emacs\\bin\\emacsclientw.exe"
		[HKEY_CURRENT_USER\Software\Classes\*\shell\emacsopencurrentframe\command]
		@="C:\\sciezka-do-emacs\\bin\\emacsclientw.exe -n --alternate-editor=C:\\sciezka-do-emacs\\bin\\runemacs.exe \"%1\""

		;; Open file in new frame
		[HKEY_CURRENT_USER\Software\Classes\*\shell\emacsopennewframe]
		@="&Emacs: Edit in new window"
		"icon"="C:\\sciezka-do-emacs\\bin\\emacsclientw.exe"
		[HKEY_CURRENT_USER\Software\Classes\*\shell\emacsopennewframe\command]
		@="C:\\sciezka-do-emacs\\bin\\emacsclientw.exe -n --alternate-editor=C:\\sciezka-do-emacs\\bin\\runemacs.exe -c \"%1\""

	**Uwaga:** w przypadku instalacji do systemowych folderów ze spacją w nazwie, użyj odpowiedniego  aliasu zgodnego z formatem 8.3, np. folderowi `Program Files` odpowiada `PROGRA~1`. 
