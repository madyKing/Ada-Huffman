with code; use code;
with Ada.Streams.Stream_IO;

-- Dictionnaire dont les cles sont des caracteres, et les valeurs
-- un code binaire associe (par defaut)

package Dico is

        -- Informations associees a un caractere
        type Info_Caractere is record
           Code : Code_Binaire := Cree_Code;
           Occur : Natural := 1;
                -- A modifier/completer selon les besoins!
        end record;

        -- La structure du dictionnaire lui-meme, privee
        type Dico_Caracteres is private;

        Caractere_Absent : exception;

        -- Cree un dictionnaire D, initialement vide
        function Cree_Dico return Dico_Caracteres;

        -- Libere le dictionnaire D
        -- garantit: en sortie toute la memoire a ete libere, et D = null.
        procedure Libere(D : in out Dico_Caracteres);

        -- Affiche pour chaque caractere: son nombre d'occurences et son code
        -- (s'il a ete genere)
        procedure Affiche(D : in Dico_Caracteres);

        -- si D est vide, return True;
        function Est_Vide(D: Dico_Caracteres) return Boolean;

-- Ajouts d'informations dans le dictionnaire

        -- Ajouter un caractere dans le dico
        procedure Add_Car(C: in Character; D: in out Dico_Caracteres);

        -- Associe un code a un caractere
        procedure Set_Code(C : in Character; Code : in Code_Binaire;
                            D : in out Dico_Caracteres);

        -- Associe les infos associees a un caractere
        -- (operation plus generale, si necessaire)
        procedure Set_Infos(C : in Character;
                            Infos : in Info_Caractere;
                            D : in out Dico_Caracteres);

-- Acces aux informations sur un caractere

        -- retourne True sur le caractere C est present dans le D
        function Est_Present(C : Character; D : Dico_Caracteres)
                return Boolean;

        -- Retourne le code binaire d'un caractere
        --  -> leve l'exception Caractere_Absent si C n'est pas dans D
        function Get_Code(C : Character; D : Dico_Caracteres)
                 return Code_Binaire;

        -- Retourne le caractere
        function Get_Key(D: Dico_Caracteres) return Character;

        -- Retourne le pointer sur record suivant
        function Get_Next(D: Dico_Caracteres) return Dico_Caracteres;

        -- Retourne les infos associees a un caractere
        --  -> leve l'exception Caractere_Absent si C n'est pas dans D
        function Get_Infos(C : Character; D : Dico_Caracteres)
                 return Info_Caractere;


-- Statistiques sur le dictionnaire (au besoin)

        -- Retourne le nombre de caracteres differents dans D
        function Nb_Caracteres_Differents(D : in Dico_Caracteres) return Natural;

        -- Retourne le nombre total de caracteres
        --  =  somme des nombre d'occurences de tous les caracteres de D
        function Nb_Total_Caracteres(D : in Dico_Caracteres) return Natural;


private
        -- Le type Dico_Caracteres_Interne doit etre defini dans le corps
        -- du package C(dico.adb)
        type Dico_Caracteres_Interne;

        type Dico_Caracteres is access Dico_Caracteres_Interne;


end Dico;

