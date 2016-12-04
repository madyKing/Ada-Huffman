with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;



package body code is

   type Code_Binaire_Interne is record
      Val: Bit;
      Suiv: Code_Binaire;
   end record;

   procedure Liberer is new Ada.Unchecked_Deallocation(Code_Binaire_Interne, Code_Binaire);


   -- Retourne true si Code vide, false si non
   function Est_Vide(C: in Code_Binaire) return Boolean is
   begin
      if C = null then
         return True;
      end if;
      return False;
   end Est_Vide;

   -- Cree un code initialement vide
   function Cree_Code return Code_Binaire is
   begin
      return null;
   end Cree_Code;

   -- Copie un code existant
   function Cree_Code(C : in Code_Binaire) return Code_Binaire is
      Code: Code_Binaire:= new Code_Binaire_Interne;
   begin
      if not Est_Vide(C) then
         Code.all := C.all;
         return Code;
      end if;

      return Cree_Code;
   end Cree_Code;

   -- Libere un code
   procedure Libere_Code(C : in out Code_Binaire) is
      Cou: Code_Binaire;
   begin
      while C /= null loop
         Cou:= C;
         C:= C.Suiv;
         Liberer(Cou);
      end loop;
   end Libere_Code;

   -- Retourne le nb de bits d'un code
   function Longueur(C : in Code_Binaire) return Natural is
      Comp: Integer := 0;
      Cou: Code_Binaire:= C;
   begin
      while Cou /= null loop
         Comp:= Comp + 1;
         Cou := Cou.Suiv;
      end loop;
      return Comp;
   end Longueur;

   -- Affiche un code
   procedure Affiche(C : in Code_Binaire) is
      Cou: Code_Binaire:= C;
   begin
      while Cou /= null loop
         Put(Integer(Cou.Val), 0);
         Cou:= Cou.Suiv;
      end loop;
   end Affiche;

   -- Ajoute le bit B en tete du code C
   procedure Ajoute_Avant(B : in Bit; C : in out Code_Binaire) is
   begin
      C:= new Code_Binaire_Interne'(B, C);
   end Ajoute_Avant;

   -- Ajoute le bit B en queue du code C
   procedure Ajoute_Apres(B : in Bit; C : in out Code_Binaire) is
      Cour: Code_Binaire;
   begin
      if Est_Vide(C) then
         C:= new Code_Binaire_Interne'(B, C);
      else
         Cour := C;
         while Cour.Suiv /= null loop
            Cour := Cour.Suiv;
         end loop;
         Cour.Suiv := new Code_Binaire_Interne'(B, null);
      end if;
   end Ajoute_Apres;

   -- ajoute les bits de C1 apres ceux de C
   procedure Ajoute_Apres(C1 : in Code_Binaire; C : in out Code_Binaire) is
      Cou: Code_Binaire:= C1;
   begin
      while Cou /= null loop
         Ajoute_Apres(Cou.Val, C);
         Cou:= Cou.Suiv;
      end loop;
   end Ajoute_Apres;

   -- Supprime le dernier bit du code C
   procedure Supprime_Queue(C: in out Code_Binaire) is
      Cou: Code_Binaire;
   begin
      if C = null then
         raise Code_Vide;
      end if;

      if C.Suiv = null then
         Liberer(C);
      else
         Cou := C;
         while Cou.Suiv.Suiv /= null loop
            Cou := Cou.Suiv;
         end loop;
         Liberer(Cou.Suiv);
      end if;
   end Supprime_Queue;

   ----------------------------------------------------------------------------------------------
   -- ITERATOR
   ----------------------------------------------------------------------------------------------
   type Iterateur_Code_Interne is record
      Code: Code_Binaire;
   end record;

   procedure Liberer_It is new Ada.Unchecked_Deallocation(Iterateur_Code_Interne, Iterateur_Code);

   -- Cree un iterateur initialise sur le premier bit du code
   function Cree_Iterateur(C : Code_Binaire) return Iterateur_Code is
      It: Iterateur_Code;
   begin
      It:= new Iterateur_Code_Interne'(Code => new Code_Binaire_Interne);
      Ajoute_Apres(C, It.Code);
      return It;
   end Cree_Iterateur;


   -- Libere un iterateur (pas le code parcouru!)
   procedure Libere_Iterateur(It : in out Iterateur_Code) is
   begin
      Libere_Code(It.Code);
      Liberer_It(It);
   end Libere_Iterateur;


   -- Retourne True s'il reste des bits dans l'iteration
   function Has_Next(It : Iterateur_Code) return Boolean is
   begin
      if It.Code.Suiv /= null then
         return True;
      end if;
      return False;
   end Has_Next;


   -- Retourne le prochain bit et avance dans l'iteration
   -- Leve l'exception Code_Entierement_Parcouru si Has_Next(It) = False
   function Next(It : Iterateur_Code) return Bit is
   begin
      if not Has_Next(It) then
         raise Code_Entierement_Parcouru;
      end if;
      It.Code:= It.Code.Suiv;
      return It.Code.Val;
   end Next;

end code;

