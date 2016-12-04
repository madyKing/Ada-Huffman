with Dico;use Dico;
with code; use code;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_Dico is
   Dict : Dico_Caracteres ;
   MonCode1,MonCode2,MonCode3,MonCode4, MonCode5 : Code_Binaire ;
   Car1 : Character:= 'C';
   Car2 : Character:= 'T';
   Car3 : Character:= 'U';
   Car4 : Character:= 'D';
   Info4: Info_Caractere;

begin
   MonCode1 := Cree_Code;
   MonCode2 := Cree_Code;
   MonCode3 := Cree_Code;
   MonCode4 := Cree_Code;

   for I in 0..3 loop
     Ajoute_Apres(ZERO, MonCode1);
   end loop;
   Ajoute_Apres(UN, MonCode1);  -- ça marche pas ça pourquoi ?
   Ajoute_Apres(ZERO, MonCode1);
   Ajoute_Apres(UN, MonCode2);
   Ajoute_Apres(UN, MonCode2);
   Ajoute_Apres(ZERO, MonCode2);
   Ajoute_Apres(ZERO, MonCode3);
   Ajoute_Apres(ZERO, MonCode3);
   Ajoute_Apres(UN, MonCode3);
   Ajoute_Apres(ZERO, MonCode3);
   Ajoute_Apres(ZERO, MonCode4);
   Ajoute_Apres(UN, MonCode4);
   Ajoute_Apres(ZERO, MonCode4);
   Ajoute_Apres(UN, MonCode3);
   Ajoute_Apres(UN, MonCode3);

   --Info4.Code := MonCode4;

   Dict := Cree_Dico;
   Set_Code(Car1, MonCode1, Dict);
   Affiche(Dict);
   Set_Code(Car2, MonCode2, Dict);
   Set_Code(Car3, MonCode3, Dict);
   Set_Code(Car3, MonCode3, Dict);
   Set_Code(Car3, MonCode3, Dict);
   Set_Infos(Car4, Info4, Dict);
   Set_Infos(Car4, Info4, Dict);
   Add_Car(Car4, Dict);
   Add_Car(Car4, Dict);
   Add_Car(Car4, Dict);
   ---affichage
   Affiche(Dict);
   if Est_Present('C',Dict) then
      Put_Line(" est present");
   else
      Put_Line("Pas present");
   end if;

   Put_Line("Le nombre total de caractères différents du dico : ");
   Put(Integer'Image(Nb_Caracteres_Differents(Dict)));
   New_Line;
   Put_Line("Le nombre total de caractères du dico : ");
   Put(Integer'Image(Nb_Total_Caracteres(Dict)));
   New_Line;
   MonCode5 := Get_Code('C',Dict);
   Affiche(MonCode5);

end;


