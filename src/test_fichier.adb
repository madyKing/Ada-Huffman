with Dico;use Dico;
with code; use code;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

procedure Test_Fichier is
   F :  Ada.Streams.Stream_IO.File_Type;
   Flux : Ada.Streams.Stream_IO.Stream_Access;
   C : Character;

   MonCode1 : Code_Binaire := Cree_Code;
   Dict : Dico_Caracteres := Cree_Dico;
begin
   Open(F,In_File,"fichier.txt");
   Flux := Stream(F);

   Ajoute_Apres(UN, MonCode1);
   Ajoute_Apres(ZERO, MonCode1);
   Ajoute_Apres(ZERO, MonCode1);
   while not End_Of_File(F) loop
      C := Character'Input(Flux);
      Add_Car(C, Dict);
   end loop;
   Affiche(Dict);
end Test_Fichier;
