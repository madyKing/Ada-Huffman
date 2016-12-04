with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Text_Io; use Ada.Text_Io;

-- Exemple de lecture/ecriture de donnes dans un fichier,
-- a l'aide de flux Ada.

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

-- Le principe est simple:
--  1) ouvrir un fichier, en lecture ou ecriture,
--  2) puis ouvrir un "flux de donnees" (Stream) relie au fichier,
--     memoire tampon dans laquelle lire/ecrire les donnees.
--  3) Lecture a l'aide de la fonction Input d'un type de donnees:
--       Ex: I := Integer'Input(Flux), C := Character'Input(Flux), ...
--  4) Ecriture a l'aide de la fonction Output d'un type de donnees:
--       Ex: Integer'Output(Flux, I), Character'Output(Flux, C), ...
--  5) Plusieurs exception peuvent etre levees:
--      ADA.IO_EXCEPTIONS.NAME_ERROR -> pb d'ouverture de fichier
--      ADA.IO_EXCEPTIONS.END_ERROR -> lecture apres EOF
--      ...
--
--
-- Pour l'exemple, on cree un fichier contenant des donnees de
-- differents types:
--   -> un Integer de valeur -12345   (code sur 4 octets)
--   -> une variable de type Octet    (code sur 1 octet)
--   -> les Character 'a', 'b' et 'c' (codes sur 1 octet)
-- Les donnees sont ensuites lues dans le fichier et affichees.
--
-- La taille du fichier doit etre de 8 octets.
-- On peut regarder son contenu binaire a l'aide de la commande od:
--   $ od -t x1 exemple_io.txt
--   $ 0000000 c7 cf ff ff 2a 61 62 63
--     0000010
-- On retrouve bien l'entier 0xfffffc7c (codage en little endian ici)
-- puis l'octet 42 (0x2a)
-- puis les trois caracteres (61 est le code ASCII de 'a')


procedure exemple_IO is

	type Octet is new Integer range 0 .. 255;
	for Octet'Size use 8; -- permet d'utiliser Octet'Input et Octet'Output,
	                      -- pour lire/ecrire un octet dans un flux
	
	-- Lit dans un fichier ouvert en lecture, et affiche les valeurs lues
	procedure Lecture(Nom_Fichier : in String) is
		Fichier : Ada.Streams.Stream_IO.File_Type;
		Flux : Ada.Streams.Stream_IO.Stream_Access;
		C : Character;
	begin
		Open(Fichier, In_File, Nom_Fichier);
		Flux := Stream(Fichier);

		Put("Lecture des donnees: ");

		Put(Integer'Input(Flux));
		Put(", ");
		Put(Integer(Octet'Input(Flux))); -- cast necessaire Octet -> Integer
		
		-- lecture tant qu'il reste des caracteres
		while not End_Of_File(Fichier) loop
			C := Character'Input(Flux); 
			Put(", "); Put(C);
		end loop;

		Close(Fichier);
	end Lecture;


	-- Ecrit dans un fichier ouvert en ecriture
	procedure Ecriture(Nom_Fichier : in String) is
		Fichier : Ada.Streams.Stream_IO.File_Type;
		Flux : Ada.Streams.Stream_IO.Stream_Access;
		I1 : Integer := -12345;
		I2 : Integer := 1;
		O : Octet := 42;
	begin
		Create(Fichier, Out_File, Nom_Fichier);
		Flux := Stream(Fichier);

		Put("Ecriture des donnees: ");
		Put(I1); Put(", ");
		Put(Integer(O)); Put(", ");
		Put('a'); Put(", ");
		Put('b'); Put(", ");
		Put('c'); Put(", ");
		new_Line;

		Integer'Output(Flux, I1);
		Octet'Output(Flux, O);
		Character'Output(Flux, 'a');
		Character'Output(Flux, 'b');
		Character'Output(Flux, 'c');
		
		Close(Fichier);
	end Ecriture;


begin
	Ecriture("exemple_io.txt");
	Lecture("exemple_io.txt");	
end exemple_IO;

