using System.Linq;
//system.Linq is necessary in order for this script to work properly
using UnityEngine;

public class TriggerReset : StateMachineBehaviour
{
    //This function makes sure that all triggers are reset in the beginning of a new state, which ensures that no triggers can be stored throughout next states. Bools should be used for that instead.
    //This script is assigned in the base layer of the animator, check playercontroller for reference.

    public override void OnStateEnter(Animator animator, AnimatorStateInfo animatorStateInfo, int layerIndex)
    {
        foreach (var parameter in animator.parameters.Where(parameter => parameter.type == AnimatorControllerParameterType.Trigger))
        {
            animator.ResetTrigger(parameter.name);
        }
    }

}
