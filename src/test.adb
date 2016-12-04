with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

procedure Deallocation_Sample is

   type Vector     is record
      A: Integer;
   end record;

   type Vector_Ref is access Vector;

   procedure Free_Vector is new Ada.Unchecked_Deallocation
     (Vector, Vector_Ref);

   VA, VB: Vector_Ref;
   V     : Vector;

begin

   VA     := new Vector'(A => 1);
   VB     := VA;  -- points to the same location as VA

   VA.all := (others => 2);

   --  ... Do whatever you need to do with the vector

   Free_Vector (VA); -- The memory is deallocated and VA is now null

   V := VB.all;  -- VB is not null, access to a dangling pointer is erroneous
   VB.A := 5;
   Put(VB.A);
end Deallocation_Sample;
