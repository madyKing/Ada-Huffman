-- paquetage generique de file de priorite
-- Les priorites sont munies d'un ordre total "Est_Prioritaire"

generic
        -- le type representant une donnee de la file
        type Donnee is private;
        -- le type representant la priorite d'une donnee
        type Priorite is private;
        -- et l'operateur d'ordre sur ce type
        with function Est_Prioritaire(P1, P2: Priorite) return Boolean;
        with procedure PutD(D: Donnee);
        with procedure PutP(P: Priorite);
package File_Priorite is

        type File_Prio is private;

        File_Prio_Pleine, File_Prio_Vide: exception ;

        -- Cree et retourne une nouvelle file, initialement vide
        -- et de capacite maximale Capacite
        function Cree_File return File_Prio;

        -- Libere une file de priorite.
        -- garantit: en sortie toute la memoire a ete libere, et F = null.
        procedure Libere_File(F : in out File_Prio);

        -- retourne True si la file est vide, False sinon
        function Est_Vide(F: in File_Prio) return Boolean;

        -- retourne True si la file est pleine, False sinon
--        function Est_Pleine(F: in File_Prio) return Boolean;

        -- si not Est_Pleine(F)
        --   insere la donnee D de priorite P dans la file F
        -- sinon
        --   leve l'exception File_Pleine
        procedure Insere(F : in out File_Prio; D : in Donnee; P : in Priorite);

        -- si not Est_Vide(F)
        --   supprime la donnee la plus prioritaire de F.
        --   sortie: D est la donnee, P sa priorite
        -- sinon
        --   leve l'exception File_Vide
        procedure Supprime(F: in out File_Prio; D: out Donnee; P: out Priorite);

        -- si not Est_Vide(F)
        --   retourne la donnee la plus prioritaire de F (sans la
        --   sortir de la file)
        --   sortie: D est la donnee, P sa priorite
        -- sinon
        --   leve l'exception File_Vide
        procedure Prochain(F: in File_Prio; D: out Donnee; P: out Priorite);

        -- si not Est_Vide(F)
        --   affiche la donnee et la priorite associe
        -- sinon
        --   leve l'exception File_Vide
        procedure Affiche(F: in File_Prio);

private
        -- Le type File_Interne doit etre defini dans le corps du package
        -- (file_priorite.adb)
        type File_Interne;
        type File_Prio is access File_Interne;

end File_Priorite;

