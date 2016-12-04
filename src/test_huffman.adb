with code; use code;
with dico; use dico;
with file_priorite ;
with huffman; use huffman;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_Huffman is
   H : Arbre_Huffman;
  -- C : Character ;
  -- Compt : Positive ;
   Dico, Dic : Dico_Caracteres := Cree_Dico ;
   --F :  Ada.Streams.Stream_IO.File_Type;
   --Flux : Ada.Streams.Stream_IO.Stream_Access;

begin
   --Open(F, Out_File, "fich.txt");
   --Flux := Stream(F);
   H := Cree_Huffman("lorem.txt",Dic);
   Put_Line("Affichage du dictionnaire");
   Genere_Dictionnaire(H,Dico);
   Affiche(Dic);
   Put_Line("Affichage de l'arbre");
   Affiche(H);
    --    Compt := Ecrit_Huffman(H,Flux);
      --  Put_Line("le nombre d'octets écrit est : ");
      --  Put(Compt);
      --  while not End_Of_File(F) loop
--         C := Character'Input(Flux);
--         Put(C); Put(", ");
 --       end loop;
  --      New_Line;
    --    Dico := Genere_Dictionnaire(H);
     --   Put_Line("Affichage du dico");
      --  Affiche(Dico);

end Test_Huffman;











