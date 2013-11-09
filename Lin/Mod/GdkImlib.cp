MODULE LinGdkImlib ["libgdk_imlib.so"];

	IMPORT LinLibc, LinGdk;
	
	TYPE 
		GdkImlibImage* = POINTER TO RECORD [untagged]
			rgb_width*, rgb_height*: INTEGER;
			rgb_data*, alpha_data*, filename*: LinLibc.PtrSTR;
			(* more fields here...so the size of this structure is not correct... *)
		END;
	
	PROCEDURE gdk_imlib_load_image* (filename: LinLibc.PtrSTR): GdkImlibImage; 
	PROCEDURE gdk_imlib_move_image* (image: GdkImlibImage): LinGdk.GdkPixmap;
	PROCEDURE gdk_imlib_move_mask* (image: GdkImlibImage): LinGdk.GdkBitmap;
	PROCEDURE gdk_imlib_render* (image: GdkImlibImage; width, height: INTEGER);

END LinGdkImlib.


LinGdk

void choix_theme(GtkWidget *widget, gpointer data)
{
 GdkImlibImage *main_image;
 GdkPixmap *main_picture;
 GdkBitmap *main_picture_mask;
 GtkWidget *logo;
 GtkWidget *fixed1, *texte1;
 GtkWidget *button1;

 gtk_sess *sess = (gtk_sess *)data;

 main_image=gdk_imlib_load_image("./Art44.jpg");
 gdk_imlib_render(main_image, 220, 110);
 main_picture = gdk_imlib_move_image(main_image);
 main_picture_mask = gdk_imlib_move_mask(main_image);
 logo = gtk_pixmap_new(main_picture, main_picture_mask);

 // sess->pref is the window where I display the background
picture and the label
 sess->pref = gtk_window_new(GTK_WINDOW_TOPLEVEL);
 gtk_widget_set_usize(sess->pref, 220, 110);
 gtk_window_set_policy(GTK_WINDOW(sess->pref), FALSE,
FALSE, FALSE);
 gtk_widget_show(sess->pref);

 fixed1 = gtk_fixed_new (); 
 gtk_widget_ref (fixed1);
 gtk_widget_show (fixed1);
 gtk_container_add(GTK_CONTAINER(sess->pref), fixed1); 
 gtk_fixed_put(GTK_FIXED(fixed1), logo, 0, 0);
 gtk_widget_show(logo);

 texte1 = gtk_label_new ("Choix de la Skin du Player:");
 gtk_widget_set_name(texte1, "label_choix_skin");
 gtk_widget_show (texte1);
 gtk_fixed_put (GTK_FIXED (fixed1), texte1, 25, 12);

 // concerning the button there is no problem , when I move
the window
it doesn't disappear (I think it's normal, but why is it
different for the label ?) 
 button1 = gtk_button_new_with_label(VALIDER);
 gtk_widget_set_usize (button1, 50, 25);
 gtk_widget_set_style (button1, style[GREEN]);
 gtk_widget_show (button1);
 gtk_fixed_put (GTK_FIXED (fixed1), button1, 45, 75);
 gtk_signal_connect(GTK_OBJECT(button1), "clicked",
GTK_SIGNAL_FUNC(change_theme), (gpointer) sess);
 

