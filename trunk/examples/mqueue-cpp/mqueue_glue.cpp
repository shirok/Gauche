//
// mqueue_glue.cpp
//
//   The mqueue-cpp extension module to show how to embed external C++
//   library in Gauche.
//   This file works as a 'glue' between the external C++ library
//   (mqueue.h, mqueue.cpp) and Gauche extension.  If you're writing
//   a bridge for the third party C++ library, the contents of this
//   file is the stuff you're supposed to write.
//

#include "mqueue_glue.h"

ScmClass *MQueueClass;

static void mqueue_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    MQueue *q = MQUEUE_UNBOX(obj);
    const char *queue_name = q->getName().c_str();
    Scm_Printf(out, "#<mqueue \"%s\">", queue_name);
}

static void mqueue_cleanup(ScmObj obj)
{
    MQueue *q;
    q = MQUEUE_UNBOX(obj);
    delete q;
}

extern void Scm_Init_mqueue_lib(ScmModule*);

void Scm_Init_mqueue_cpp()
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(mqueue_cpp);

    /* Create example.mqueue-cpp module */
    mod = SCM_MODULE(SCM_FIND_MODULE("example.mqueue-cpp", TRUE));
    
    /* Create the foreign pointer class <mqueue-cpp>.
       The flag SCM_FOREIGN_POINTER_KEEP_IDENTITY makes Gauche to keep
       one-to-one mapping between the foreign object pointer (MQueue*)
       and its wrapping ScmObj.  With this flag, you can assume that
       when mqueue_cleanup is called, no other ScmForeignPointer object
       is pointing to the same MQueue*, thus you can delete it safely. */
    MQueueClass =
        Scm_MakeForeignPointerClass(mod, "<mqueue>",
                                    mqueue_print,
                                    mqueue_cleanup,
                                    SCM_FOREIGN_POINTER_KEEP_IDENTITY|SCM_FOREIGN_POINTER_MAP_NULL);

    /* Initialize stub functions */
    Scm_Init_mqueue_lib(mod);
}
