
BINDIR=./bin

OCAMLC=ocamlc
OCAMLLINK=ocamlc

LINKCAMLP4=-I +../type_conv/ pa_type_conv.cma -I +../sexplib/ pa_sexp_conv.cma -linkall

LINKLIBS=ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma dynlink.cma camlp4o.cma
OCAMLCFLAGS=-g -I +compiler-libs -I +camlp4
OCAMLLINKFLAGS=-g -I +compiler-libs -I +camlp4 ${LINKLIBS} ${LINKCAMLP4}

%.cmi: %.mli
	${OCAMLC} ${OCAMLCFLAGS} -c $<

%.cmo %.cmi: %.ml
	${OCAMLC} ${OCAMLCFLAGS} -c $<

all: ocaml-with-pp

clean:
	-rm *.cmi *.cmo
	-rm ocaml-with-pp

install: all
	-mkdir -p ${BINDIR}
	cp ocaml-with-pp ${BINDIR}/ocaml-with-pp

ocaml-with-pp: pp_pparse.cmo pp_compile.cmo pp_main.cmo
	${OCAMLLINK} ${OCAMLLINKFLAGS} -o $@ $^

pp_pparse.cmo: pp_pparse.cmi

pp_compile.cmo: pp_pparse.cmo pp_compile.cmi

pp_main.cmo: pp_compile.cmo pp_main.cmi
