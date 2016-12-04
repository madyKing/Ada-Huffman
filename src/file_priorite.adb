with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body File_Priorite is

   type Donnee_Abstraite is record
      D: Donnee;
      P: Priorite;
   end record;

   type File_Interne is record
      D: Donnee;
      P: Priorite;
      Suiv: File_Prio;
   end record;

   procedure Liberer is new Ada.Unchecked_Deallocation(File_Interne, File_Prio);

   -- Cree et retourne une nouvelle file, initialement vide
   -- et de capacite maximale Capacite
   function Cree_File return File_Prio is
   begin
      return null;
   end Cree_File;

   -- Libere une file de priorite.
   -- garantit: en sortie toute la memoire a ete libere, et F = null.
   procedure Libere_File(F : in out File_Prio) is
      Cou: File_Prio;
   begin
      while F /= null loop
         Cou := F;
         F:= F.Suiv;
         Liberer(Cou);
      end loop;
   end Libere_File;

   -- retourne True si la file est vide, False sinon
   function Est_Vide(F: in File_Prio) return Boolean is
   begin
      if F = null then
         return True;
      end if;
      return False;
   end Est_Vide;

   --   insere la donnee D de priorite P dans la file F
   procedure Insere(F : in out File_Prio; D : in Donnee; P : in Priorite) is
      Cou: File_Prio := F;
   begin
      if Est_Vide(F) then -- on insere directement l'element
         F := new File_Interne'(D, P, F);
      elsif Est_Prioritaire(P, F.P) then -- priorite plus importante donc on l'insere en tete
         F := new File_Interne'(D, P, F);
      else
         while Cou.Suiv /= null and then Est_Prioritaire(Cou.Suiv.P, P) loop
            Cou := Cou.Suiv;
         end loop;
		 --sinon on l'insere a la bonne place
         Cou.Suiv := new File_Interne'(D, P, Cou.Suiv);
      end if;
   end Insere;


   -- si not Est_Vide(F)
   --   supprime la donnee la plus prioritaire de F.
   --   sortie: D est la donnee, P sa priorite
   -- sinon
   --   leve l'exception File_Vide
   procedure Supprime(F: in out File_Prio; D: out Donnee; P: out Priorite) is
      Cou: File_Prio;
   begin
      if Est_Vide(F) then
         raise File_Prio_Vide;
      end if;

      D:= F.D;
      P:= F.P;
      Cou:= F;
      F:= F.Suiv;
      Liberer(Cou);
   end Supprime;

   -- si not Est_Vide(F)
   --   affiche la donnee et la priorite associe
   procedure Affiche(F: in File_Prio) is
      Cou: File_Prio := F;
   begin
      if Est_Vide(F) then
         raise File_Prio_Vide;
      end if;
      while Cou /= null loop
         Put("Donnee: ");
         PutD(Cou.D);
         Put(" - Priorite: ");
         PutP(Cou.P);
         New_Line;
         Cou := Cou.Suiv;
      end loop;
   end Affiche;


   -- si not Est_Vide(F)
   --   retourne la donnee la plus prioritaire de F (sans la
   --   sortir de la file)
   --   sortie: D est la donnee, P sa priorite
   -- sinon
   --   leve l'exception File_Vide
   procedure Prochain(F: in File_Prio; D: out Donnee; P: out Priorite) is
   begin
      if Est_Vide(F) then
         raise File_Prio_Vide;
      end if;

      D:= F.D;
      P:= F.P;
   end Prochain;


end File_Priorite;
