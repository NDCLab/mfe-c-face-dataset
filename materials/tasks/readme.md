
In the initial version of the experiment (v1), participants 1 through 22 were presented with image pairs in a randomized order. However, a bug in the 'Code_6' code block in surprise_task routine prevented the intended randomization of the images **within each pair**. This occurred because the code's directory was not correctly set on the data collection computer, resulting in participants 1 through 22 receiving the same fixed set of image pairs. 
Note: The image pairs were still correctly presented in a randomized order.

This issue was rectified in the updated version of the experiment (v2), where the images within each pair were also randomized, ensuring that each participant encountered a unique set of image pairings.