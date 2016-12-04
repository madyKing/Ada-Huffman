with code; use code;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

--Implémentation du package Dico
--Association entre les caractères et les codes binaires les représentant

package body dico is

   --Definition du type Dico_Carateres_Interne 
   type Dico_Caracteres_Interne is record
      Key : Character;
      Infos : Info_Caractere;
      Suiv : Dico_Caracteres;
   end record;

   procedure Liberer is new Ada.Unchecked_Deallocation(Dico_Caracteres_Interne, Dico_Caracteres);

   --Creation d'un dico initialement vide
   function Cree_Dico return Dico_Caracteres is
   begin
      return null;
   end Cree_Dico;

   --Procedure pour liberer un dico, 
   --Assure la liberation de memoire à la fin
   procedure Libere(D : in out Dico_Caracteres) is
      Cou: Dico_Caracteres := D;
   begin
      while (D /= null) loop
         Cou := D;
         Liberer(Cou);
         D := D.Suiv;
      end loop;
   end;
   --Permet de savoir si un dico est vide ou pas
   function Est_Vide(D: Dico_Caracteres) return Boolean is
   begin
      if D = null then
         return True;
      end if;
      return False;
   end Est_Vide;


   --Procedure pour afficher un caractere et ses infos
   --infos : Code_Binaire & occurence
   procedure Affiche(D : in Dico_Caracteres) is
      Cou : Dico_Caracteres := D;
   begin
      while(Cou /= null) loop
         Put("" & Cou.Key);
         Put(" : ");
         Affiche(Cou.Infos.Code);
         Put_Line(", " & Integer'Image(Cou.Infos.Occur));
         Cou := Cou.Suiv;
      end loop;
   end;

----------------------- Operations sur le dictionaire-------------------------------------------
   -- Ajoute un caractere dans le dico
   -- Si le caractere existe deja, incremente son occur
   procedure Add_Car(C: in Character; D: in out Dico_Caracteres) is
      Cour: Dico_Caracteres;
   begin
      if D = null then
         D:= new Dico_Caracteres_Interne;
         D.Key := C;
      else
         Cour := D;
         if not Est_Present(C, D) then
            while Cour.Suiv /= null loop
               Cour := Cour.Suiv;
            end loop;
            Cour.Suiv := new Dico_Caracteres_Interne;
            Cour.Suiv.Key := C;
         else
            while Cour.Key /= C loop
               Cour := Cour.Suiv;
            end loop;
            Cour.Infos.Occur := Cour.Infos.Occur + 1;
         end if;
      end if;
   end Add_Car;

   -- procedure pour associer un code à un caractere
   procedure Set_Code(C : in Character; Code : in Code_Binaire; D : in out Dico_Caracteres) is
      Cour: Dico_Caracteres;
      Info : Info_Caractere;
   begin
      if D = null then -- on cree dans ce cas un nouveau element, le seul du dico
         D:= new Dico_Caracteres_Interne;
         D.Key := C;
         Ajoute_Apres(Code, D.Infos.Code);
      else
         Cour := D;
         if not Est_Present(C, D) then
            while Cour.Suiv /= null loop
               Cour := Cour.Suiv;
            end loop; -- on est a la fin de D, donc on peut ajouter un nouveau element
            Ajoute_Apres(Code, Info.Code);
            Info.Occur := 1;
            Cour.Suiv := new Dico_Caracteres_Interne'(Key =>C,Infos => Info, Suiv => null);
         end if;
      end if;
   end;

   -- procedure pour associer infos(code,occurence) a un caractere
   procedure Set_Infos(C : in Character; Infos : in Info_Caractere; D : in out Dico_Caracteres) is
      Cour: Dico_Caracteres;
      Info : Info_Caractere;
   begin
      if D = null then
         D:= new Dico_Caracteres_Interne;
         D.Key := C;
         D.Infos := Infos;
      else
         Cour := D;
         if not Est_Present(C,D) then
                while Cour.Suiv /= null loop
                Cour := Cour.Suiv;
                end loop;
                Cour.Suiv := new Dico_Caracteres_Interne'(Key =>C,Infos => Infos, Suiv => null);
         else
                while Cour.Key /= C loop
                        Cour := Cour.Suiv;
                end loop;
                Cour.Infos := Infos;
         end if;
      end if;
   end Set_Infos;

   -- verifying the presence of an element
   -- return true if element exists, false otherwise
   function Est_Present(C : Character; D : Dico_Caracteres) return Boolean is
      Cou: Dico_Caracteres := D;
   begin
      while (Cou /= null) loop
         if Cou.Key = C then
            return True;
         end if;
         Cou:= Cou.Suiv;
      end loop;
      return False;
   end;

   -- retrieving the code of a character
   --ExceptionHandler --> Caractere_Absent
   function Get_Code(C : Character; D : Dico_Caracteres) return Code_Binaire is
      Cou: Dico_Caracteres := D;
   begin
      if not Est_Present(C, D) then
         raise Caractere_Absent;
      else
         while(Cou /= null) loop
            if Cou.Key = C then
               return Cou.Infos.Code;
            end if;
            Cou := Cou.Suiv;
         end loop;
      end if;
   end;

   -- Retourne le caractere
   function Get_Key(D: Dico_Caracteres) return Character is
   begin
      return D.Key;
   end Get_Key;

   --retrieving the info of a character
   --ExceptionHandler --> Caractere_Absent
   function Get_Infos(C : Character; D : Dico_Caracteres) return Info_Caractere is
      Cou: Dico_Caracteres := D;
   begin
       if not Est_Present(C, D) then
         raise Caractere_Absent;
       else
          while (Cou /= null) loop
             if Cou.Key = C then
                return Cou.Infos;
             end if;
             Cou := Cou.Suiv;
          end loop;
       end if;
   end;

   -- Retourne le pointeur sur record suivant
   function Get_Next(D: Dico_Caracteres) return Dico_Caracteres is
   begin
      return D.Suiv;
   end Get_Next;

   -- function to know the number of different characters in the dict
   function Nb_Caracteres_Differents(D : in Dico_Caracteres) return Natural is
      Count : Natural := 0;
      Cou : Dico_Caracteres:= D;
   begin
      while(Cou /= null) loop
         Count := Count + 1;
         Cou := Cou.Suiv;
      end loop;
      return Count;
   end;

   -- function to know the total number of characters in the dict
   function Nb_Total_Caracteres(D : in Dico_Caracteres) return Natural is
      Cou : Dico_Caracteres := D;
      Count : Natural := 0;
   begin
      while(Cou /= null) loop
         Count := Count + Cou.Infos.Occur;
         Cou := Cou.Suiv;
      end loop;
      return Count;
   end;
end dico;
