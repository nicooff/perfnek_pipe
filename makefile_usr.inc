SRC_LOC := ./
$(OBJDIR)/IO_tools.o             :$(SRC_LOC)/IO_tools.f;                         $(F77) -c $(FL2) -I./ $< -o $@
$(OBJDIR)/uparam.o               :$(SRC_LOC)/uparam.f;                           $(F77) -c $(FL2) -I./ $< -o $@
$(OBJDIR)/chkpoint.o             :$(SRC_LOC)/chkpoint.f;                         $(F77) -c $(FL2) -I./ $< -o $@
