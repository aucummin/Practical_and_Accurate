import numpy as np
import sys
import os
import zhaires
sys.path.append('/storage/home/alc6658/work/2-8-4a/bin/')

def test_run_task(antx, anty, antz) -> None:
    """
    Check that I can create and run ZHAireS sims.
    """
    # create the simulation
    #sim = zhaires.Task(directory="/tmp/", program='/home/abl/aires2/bin/Aires')

    #task_name = str(1000+antx)+'_'+str(1000+anty)+'_'+str(1000+antz)
    task_name = 'test'
    isDir = os.path.isdir(task_name)
    if isDir:
        os.chdir(task_name)
    else:
        os.mkdir(task_name)
        os.chdir(task_name)

    full_directory = os.getcwd()
    sim = zhaires.Task()
    sim.file_directory(full_directory, files = 'All')
    # and use some of the functions
    sim.task_name(task_name)
    sim.primary_particle("proton")
    sim.primary_energy(1, "EeV")
    sim.primary_zenith(80)
    sim.add_antenna(antx,anty,antz)
    #sim.primary_zenith(55.)
    #sim.add_antenna(138.,0,0)
    sim.primary_azimuth(0.0, 'Magnetic')
    sim.injection_altitude(50, "km")
    sim.ground_altitude(0, "m")
    sim.geomagnetic_field(42000,-37,0)
    sim.total_showers(1)
    sim.runs_per_process(1)
    sim.showers_per_run(1)
    sim("RandomSeed 0.128900437")
    sim.zhaires(True)
    sim.fresnel_time(True)
    sim.time_domain_bin(0.5)
    sim.thinning_energy(1e-4, 'Relative')
    #sim.thinning_w_factor(0.06)
    sim.thinning_w_factor(1)
    sim.remark("Task generated in zhaires.py/tests::test_task.py")

    # and run the simulation
    sim.run()
    os.chdir('..')

xs = np.arange(-75,80,6)
ys = np.arange(-75, 80, 6)
print(xs)
#time.sleep(500)
#for i in range(0, len(xs)):
#    for j in range(0, len(ys)):
#        test_run_task(xs[i], ys[j], 0)


test_run_task(500,0,0)
#print(xs)
#test_run_task(50,0,0)
#test_run_task(60,0,0)
