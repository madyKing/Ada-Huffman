with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with huffman; use huffman;
with dico; use dico;
with code; use code;
procedure tp_huffman is

   ------------------------------------------------------------------------------
   -- COMPRESSION
   ------------------------------------------------------------------------------

   procedure Compresse(Nom_Fichier_In, Nom_Fichier_Out : in String) is
      A: Arbre_Huffman;
      D: Dico_Caracteres;
      F_I, F_O: Ada.Streams.Stream_IO.File_Type;
      Flux_I, Flux_O: Ada.Streams.Stream_IO.Stream_Access;
      C: Character;
      Code: Code_Binaire;
      It: Iterateur_Code;
      B: Bit;
      Nb_Octet: Positive;
   begin
      -- Cree un arbre Huffman pour le codage
      Cree_Huffman(Nom_Fichier_In, D, A);
      -- Genere le dictionnaire pour eviter les parcours multiples
      Genere_Dictionnaire(A, D);

      -- Ouvrir le fichier source pour lire
      Open(F_I, In_File, Nom_Fichier_In);
      Flux_I:= Stream(F_I);

      -- Ouvrir le fichier destination pour ecrire
      Create(F_O, Out_File, Nom_Fichier_Out);
      Flux_O:= Stream(F_O);

      -- Stocke l'arbre dans le flux ouvert en ecriture
      Nb_Octet:= Ecrit_Huffman(D, Flux_O);

      -- Compresse le fichier
      while not End_Of_File(F_I) loop
         -- Prend un caractere
         C:= Character'Input(Flux_I);
         -- Cherche son code associe
         Code:= Get_Code(C, D);
         -- Fait un parcours du code de caractere via un iterateur
         It:= Cree_Iterateur(Code);
         -- Ecrit le code du caractere dans le fichier destination
         while Has_Next(It) loop
            B:= Next(It);
            Bit'Output(Flux_O, B);
         end loop;

      end loop;

      -- Ferme les fichiers apres compression
      Close(F_I);
      Close(F_O);

      -- Libere l'espace utilise
      Libere_Iterateur(It);
      Libere(D);
      Libere(A);

      -- Affiche les etats
      Put_Line("Compression finie");
      Put("Nombre de caracteres lus: ");
      Put(A.Nb_Total_Caracteres);
      New_Line;
      Put("Nombre d'octets utilise pour la tete: "); -- poids caractere et poids code associe
      Put(Nb_Octet);
      New_Line;
      Put_Line("--------------------------------------------------------------");
      New_Line;
   end Compresse;



   ------------------------------------------------------------------------------
   -- DECOMPRESSION
   ------------------------------------------------------------------------------

   procedure Decompresse(Nom_Fichier_In, Nom_Fichier_Out : in String) is
      A: Arbre_Huffman;
      Noeud_Debut: Arbre;
      F_I, F_O: Ada.Streams.Stream_IO.File_Type;
      Flux_I, Flux_O: Ada.Streams.Stream_IO.Stream_Access;
      C: Character;
      C_Trouve: Boolean;
      Code: Code_Binaire;
      It: Iterateur_Code;
      B: Bit;
      N: Natural:= 0;
   begin
   	  Put_Line("Decompression");
      -- Ouvrir le fichier source pour lire
      Open(F_I, In_File, Nom_Fichier_In);
      Flux_I:= Stream(F_I);
      -- Cree l'arbre huffman stocke dans le debut du fichier compresse
      A:= Lit_Huffman(Flux_I);
      --Put_Line("Arbre Huffman est construit");
      -- Ouvre le fichier destination pour ecrire
      Create(F_O, Out_File, Nom_Fichier_Out);
      Flux_O:= Stream(F_O);
	  
	  Noeud_Debut:= A.A;
      -- Genere le code pour decoder : On decode pour chaque 8 bit
      while not End_Of_File(F_I) loop
         while not End_Of_File(F_I) and N < 8 loop
            B:= Bit'Input(Flux_I);
            Ajoute_Apres(B, Code);
            N := N + 1;
         end loop;
         
         It:= Cree_Iterateur(Code);
         -- Decode le fichier source
         while Has_Next(It) loop
         -- Parcours de l'arbre
            Get_Caractere(It, Noeud_Debut, C_Trouve, C);
            -- si on trouve un caractere
            if C_Trouve then
               -- Ecrit-le dans le fichier destination
               Character'Output(Flux_O, C);
               -- Mis a jour du noeud debut pour parcourrir
               Noeud_Debut:= A.A;
            end if;
         end loop;
         N:= 0;
         Libere_Code(Code);
      end loop;
      -- Ferme les fichiers apres decompression
      Close(F_I);
      Close(F_O);
      -- Libere l'espace utilise
      Libere_Iterateur(It);
      Libere(A);
   end Decompresse;


   ------------------------------------------------------------------------------
   -- PG PRINCIPAL
   ------------------------------------------------------------------------------

begin

   if (Argument_Count /= 3) then
      Put_Line("utilisation:");
      Put_Line("  compression : ./huffman -c fichier.txt fichier.comp");
      Put_Line("  decompression : ./huffman -d fichier.comp fichier.comp.txt");
      Set_Exit_Status(Failure);
      return;
   end if;

   if (Argument(1) = "-c") then
      Compresse(Argument(2), Argument(3));
   else
      Decompresse(Argument(2), Argument(3));
   end if;

   Set_Exit_Status(Success);

end tp_huffman;

