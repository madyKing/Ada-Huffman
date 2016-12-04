with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with code;
use code;

procedure Test_Code is
   Bit_Chaine: Code_Binaire;
   Chaine2: Code_Binaire;
   It: Iterateur_Code;
   B: Bit;
begin
   Bit_Chaine:= Cree_Code;

   for I in 0..5 loop
      Ajoute_Apres(UN, Bit_Chaine);
   end loop;
   Ajoute_Apres(ZERO, Bit_Chaine);
   Ajoute_Apres(ZERO, Bit_Chaine);
   Ajoute_Apres(ZERO, Bit_Chaine);

   Ajoute_Apres(UN, Bit_Chaine);
   Ajoute_Apres(UN, Bit_Chaine);

   Affiche(Bit_Chaine);
   It:= Cree_Iterateur(Bit_Chaine);
   while Has_Next(It) loop
      New_Line;
      B:= Next(It);
      Put(Integer'Image(B));
   end loop;

   Chaine2:= Cree_Code;
   Affiche(Chaine2);
end Test_Code;
