MODULE LinTestPic;

	(* Illustrates how to use gdk_imlib to show pictures. A jpeg-picture saved in /home/picture.jpg is needed for the example to run *)

	IMPORT LinGdk, LinGdkImlib, LinGtk;

	PROCEDURE Do*;
		VAR img: LinGdkImlib.GdkImlibImage; pm: LinGdk.GdkPixmap; bm: LinGdk.GdkBitmap; pic, win: LinGtk.GtkWidget;
	BEGIN
		img := LinGdkImlib.gdk_imlib_load_image("/home/bengt/blackbox/linwork/Lin/Rsrc/testpic.jpg");
		LinGdkImlib.gdk_imlib_render(img, img.rgb_width, img.rgb_height);
		pm := LinGdkImlib.gdk_imlib_move_image(img);
		bm := LinGdkImlib.gdk_imlib_move_mask(img);
		pic := LinGtk.gtk_pixmap_new(pm, bm);
		win := LinGtk.gtk_window_new(LinGtk.GTK_WINDOW_TOPLEVEL);
		LinGtk.gtk_container_add(win(LinGtk.GtkContainer), pic);
		LinGtk.gtk_widget_show_all(win)
	END Do;

END LinTestPic.Do