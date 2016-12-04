with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with dico; use dico;
with code; use code;
with file_priorite;

package body Huffman is

   type Noeud is record
      Val : Character;
      Occur: Natural:= 0;
      Fg : Arbre;
      Fd : Arbre;
   end record;

   function Est_Prioritaire(P1, P2: Natural) return Boolean is
   begin
      if P1 <= P2 then
         return True;
      end if;
      return False;
   end Est_Prioritaire;

   procedure PutD(D: Arbre) is
   begin
      Put(D.Val);
   end PutD;

   procedure PutP(P: Natural) is
   begin
      Put(P);
   end PutP;

   package File_Priorite_Arbre is new file_priorite(Arbre, Integer, Est_Prioritaire, PutD, PutP);
   use File_Priorite_Arbre;

   procedure Liberer_Noeud is new Ada.Unchecked_Deallocation(Noeud, Arbre);

   -- Savoir si on a à faire à une feuille
   function Est_Feuille(H : in Arbre) return boolean is
   begin
      if H.Fg = null and H.Fd = null then
         return True;
      end if;
      return False;
   end Est_Feuille;


   procedure Libere_Arbre(A: in out Arbre) is
   begin
      if Est_Feuille(A) then
         Liberer_Noeud(A);
      else
         Libere_Arbre(A.Fg);
         Libere_Arbre(A.Fd);
         Liberer_Noeud(A);
      end if;
   end Libere_Arbre;

   -- Libere l'arbre de racine A.
   -- garantit: en sortie toute la memoire a ete libere, et A = null.
   procedure Libere(H : in out Arbre_Huffman) is
   begin
      Libere_Arbre(H.A);
   end Libere;


   --Affichage de l'arbre
   procedure Affiche(H : in Arbre_Huffman) is

      procedure Sous_Procedure(A : in Arbre; C : in Code_Binaire) is
         C1, C2 : Code_Binaire := Cree_Code;
      begin
         if Est_Feuille(A) then
            Put("Feuille: ");
            Put(A.Val & " - ");
            Affiche(C);
            New_Line;
            return;
         end if;
         Ajoute_Apres(0,C1);		--pour les feuilles a gauche, on met un 0 a cote
         Sous_Procedure(A.Fg, C1);
         --Libere_Code(C1);

         Ajoute_Apres(1, C2);		--et pour les droites on met un 1 a cote
         Sous_Procedure(A.Fd, C2);
         --Libere_Code(C2);
      end Sous_Procedure;

      C : Code_Binaire := Cree_Code;
   begin
      Sous_Procedure(H.A, C);
      Libere_Code(C);
   end Affiche;

   -- Cree un arbre de Huffman a partir d'un fichier texte
   -- Cette procedure lit le fichier et compte le nb d'occurences des
   -- differents caracteres presents, puis genere l'arbre correspondant
   -- et le retourne.
   procedure Cree_Huffman(Nom_Fichier : in String; Dic: out Dico_Caracteres; 
                         A: out Arbre_Huffman) is

      F: Ada.Streams.Stream_IO.File_Type;
      Flux : Ada.Streams.Stream_IO.Stream_Access;
      C : Character;
      P1, P2 : Natural;
      Dico, D : Dico_Caracteres;
      Infos: Info_Caractere;
      File : File_Prio := Cree_File;

      H: Arbre_Huffman;
      N1, N2, Noeud_Temps: Arbre;

   begin

      Open(F, In_File, Nom_Fichier);
      Flux := Stream(F);

      Dico := Cree_Dico;
      -- Charge les caracteres dans le dictionnaire, si un caractere existe deja, 
      -- son occurence va etre incremente
      while not End_Of_File(F) loop
         C := Character'Input(Flux);
         Add_Car(C, Dico); --Dico.Infos contient les infos utiles
      end loop;

      H.Nb_Total_Caracteres := Nb_Total_Caracteres(Dico);

      -- Charge les caracteres dans la file de priorite
      D := Dico;
      while not Est_Vide(D) loop
         C:= Get_Key(D);
         Infos:= Get_Infos(C, Dico);
         Noeud_Temps := new Noeud'(C, Infos.Occur, null, null);
         Insere(File, Noeud_Temps, Infos.Occur);
         D := Get_Next(D);
      end loop;


      if Est_Vide(File) then
         raise File_Prio_Vide;	--echec dans la creation de la file
      end if;
	  -- on execute l'algorithme de creation de l'arbre de huffman
	  -- on sort d'abord les deux premiers elements de priorite superieure
	  -- pour former un nouveau noeud de priorite la somme des priorite
	  -- qu'on insere ensuite dans la file de priorite
	  -- ce qui va diminuer le nombre d'elements de 1
	  -- on repete ceci jusqu'a file vide
      loop
         Supprime(File, N1, P1);
         exit when Est_Vide(File);	-- on quitte s'il ne reste plus d'elements
         Supprime(File, N2, P2);
         Noeud_Temps := new Noeud'('N', P1 + P2, N1, N2);
         Insere(File, Noeud_Temps, P1 + P2);
      end loop;

      Libere_File(File);
      Dic:= Dico;
      Close(F);
      if H.Nb_Total_Caracteres = 1 then
         H.A := new Noeud'(' ', 0, N1, null);
      else
         H.A := N1; -- on retourne l'arbre gauche qui est exactement l'arbre de Huffman
      end if;

      A :=H;
   end Cree_Huffman;

   -- Stocke un arbre dans un flux ouvert en ecriture
   -- Le format de stockage est celui decrit dans le sujet
   -- Retourne le nb d'octets ecrits dans le flux (pour les stats)

   function Ecrit_Huffman(Dico: Dico_Caracteres; Flux : Ada.Streams.Stream_IO.Stream_Access)
                         return Positive is
      D: Dico_Caracteres;
      Nb_Octets: Positive := 4;
      C: Character;
      Infos: Info_Caractere;
   begin
      Integer'Output(Flux, Nb_Caracteres_Differents(Dico));

      D := Dico;
      while not Est_Vide(D) loop
      	 -- On recupere les infos sur chaque caractere
         C:= Get_Key(D);
         Infos:= Get_Infos(C, Dico);
         -- Ecrit chaque caractere avec son occurence
         Character'Output(Flux, C);
         Integer'Output(Flux, Infos.Occur);

         Nb_Octets:= Nb_Octets + 5; -- 1 octet pour le caractere et 4 octets pour l'entier
         D := Get_Next(D);
      end loop;
      return Nb_Octets;
   end Ecrit_Huffman;

   -- Lit un arbre stocke dans un flux ouvert en lecture
   -- Le format de stockage est celui decrit dans le sujet
   function Lit_Huffman(Flux : Ada.Streams.Stream_IO.Stream_Access)
                       return Arbre_Huffman is
      H: Arbre_Huffman;
      C: Character;
      D, Dico: Dico_Caracteres;
      Infos: Info_Caractere;
      File: File_Prio:= Cree_File;
      N1, N2, Noeud_Temps: Arbre;
      P1, P2 : Natural;

   begin
      H.Nb_Total_Caracteres:= Integer'Input(Flux);
      --on stocke les caracteres et leurs occurences dans le dico
      --pour eviter des insertions multiples
      for I in 1 .. H.Nb_Total_Caracteres loop
         C:= Character'Input(Flux);
         Infos.Occur:= Integer'Input(Flux);
         Set_Infos(C, Infos, Dico);
      end loop;

      D := Dico;
      -- on utilise une file pour ordonner ensuite les caracteres
      -- par ordre de priorite sur les occurences
      while not Est_Vide(D) loop
         C:= Get_Key(D);
         Infos:= Get_Infos(C, Dico);
         Noeud_Temps := new Noeud'(C, Infos.Occur, null, null);
         Insere(File, Noeud_Temps, Infos.Occur);
         D := Get_Next(D);
      end loop;
      -- Affiche(Dico);

      if Est_Vide(File) then
         raise File_Prio_Vide; --echec dans la creation de la file
      end if;
	  
      loop
         Supprime(File, N1, P1);
         exit when Est_Vide(File); -- on quitte s'il ne reste plus d'elements
         Supprime(File, N2, P2);
         Noeud_Temps := new Noeud'('N', P1 + P2, N1, N2);
         Insere(File, Noeud_Temps, P1 + P2);
      end loop;

      if H.Nb_Total_Caracteres = 1 then
         H.A := new Noeud'(' ', 0, N1, null);
      else
         H.A := N1;
      end if;

      return H;
   end Lit_Huffman;

   -------------------------------------------------------------------
   -- Mettre a jour le dictionnaire contenant les caractères présents
   -- dans l'arbre et leur code binaire

   procedure Genere_Dictionnaire(H : in Arbre_Huffman; Dic: in out Dico_Caracteres) is
      procedure Genere_Code(A: in Arbre; Code: in out Code_Binaire; Dico: in out Dico_Caracteres) is
         Infos: Info_Caractere;
      begin
         if Est_Feuille(A) then
            Infos.Occur := A.Occur;
            Ajoute_Apres(Code, Infos.Code);
            Set_Infos(A.Val, Infos, Dico);
         else
            --si A n'est pas une feuille, on descend récursivement pour traiter chacune des feuilles
            Ajoute_Apres(ZERO, Code); --on descends a gauche dans l'arbre
            Genere_Code(A.Fg, Code, Dico);
            Supprime_Queue(Code);
            Ajoute_Apres(UN, Code);   -- on descends sinon a droite
            Genere_Code(A.Fd, Code, Dico);
            Supprime_Queue(Code);
         end if;
      end Genere_Code;

      Code : Code_Binaire := Cree_Code;
   begin
      Genere_Code(H.A, Code, Dic);
   end Genere_Dictionnaire;

   ------ Parcours de l'arbre (decodage)

   -- Parcours a l'aide d'un iterateur sur un code, en partant du noeud A
   --  * Si un caractere a ete trouve il est retourne dans Caractere et
   --    Caractere_Trouve vaut True. Le code n'a eventuellement pas ete
   --    totalement parcouru. A est une feuille.
   --  * Si l'iteration est terminee (plus de bits a parcourir ds le code)
   --    mais que le parcours s'est arrete avant une feuille, alors
   --    Caractere_Trouve vaut False, Caractere est indetermine
   --    et A est le dernier noeud atteint.
   procedure Get_Caractere(It_Code : in Iterateur_Code;
                           A : in out Arbre;
                           Caractere_Trouve : out Boolean;
                           Caractere : out Character) is
      B: Bit;
   begin
      while Has_Next(It_Code) loop
         B:= Next(It_Code);
         if B = ZERO then --parcours a gauche
            A:= A.Fg;
         else
            A:= A.Fd; --sinon a droite
         end if;

         if Est_Feuille(A) then
            Caractere:= A.Val;
            Caractere_Trouve:= True;
            return;
         end if;
      end loop;
      Caractere_Trouve:= False; 

   end Get_Caractere;

end Huffman;

