with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with file_priorite;

procedure Test_File is
   function Est_Prioritaire(P1, P2: Integer) return Boolean is
   begin
      if P2 > P1 then
         return False;
      end if;
      return True;
   end Est_Prioritaire;

   procedure PutD(D: Character) is
   begin
      Put(D);
   end PutD;

   procedure PutP(P: Integer) is
   begin
      Put(P);
   end PutP;


   package File_Priorite_Dico is
      new file_priorite(Character, Integer, Est_Prioritaire, PutD, PutP);
   use File_Priorite_Dico;

   F: File_Prio;
   D: Character;
   P: Integer;

begin
   F:= Cree_File;
   Insere(F, 'A', 2);
   Insere(F, 'B', 3);
   Insere(F, 'C', 1);
   Insere(F, 'D', 4);
   Affiche(F);

   Put_Line("-------------------");
   Supprime(F, D, P);
   Affiche(F);
   Put(D);
   Put(P);
end Test_File;

