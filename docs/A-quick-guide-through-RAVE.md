* [A Quick Example](#a-quick-example)
* [RAVE to SUMA](#rave-to-suma)
* [Reproduce Your Findings](#reproduce-your-findings)

# A Quick Example

> Step 1: enter command `rave::init_app()` in R

*(After installing `RAVE`, you have probably tried `init_app()`.)*

Your browser will pop out.

> Step 2: Import demo subject

If you click `Select Data`, then there is a toy example `Subject_RAVE_Demo` with one valid electrode.

![Toy Example - Data Selection](https://github.com/dipterix/instrave/blob/master/img/mainapp/%5Bselect%20data%5D%20demo%20subject.PNG)

_* Demo subject that comes with RAVE_

Press `Import`, and you will see the default module `Condition Explore`.

![Screenshot of Main App](https://github.com/dipterix/instrave/blob/master/img/mainapp/%5BMain%5D%20snapshot.PNG)

_* This screenshot is generated as of 03/03/2018. The actual might be slightly different._

> Step 3: Play with default module `Condition Explore`.

The left panel is your inputs, you can change any of these parameters, and the result will be rendered in the right panels almost immediately.

Now if you choose to compare two groups: *Auditory* vs. *Visual*, you will notice that in panel `Activity over Time`, ECoG data from time 0s-0.8s has some difference. Now you want to check whether we can perform Wilcox-t test on this time span

# RAVE to SUMA


# Reproduce Your Findings