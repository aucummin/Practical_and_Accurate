import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import griddata
import os

plt.rcParams['axes.labelsize'] = 13
plt.rcParams['xtick.labelsize'] = 11
plt.rcParams['ytick.labelsize'] = 11
plt.rcParams['legend.fontsize'] = 11
plt.rcParams.update({'figure.autolayout': True})

a = 2.40160339973116e-9

def plot_comp_freq(dir):
    data_ZHS = np.loadtxt(dir+'/timefresnel-root.dat', skiprows = 10)
    data_PA = np.loadtxt(dir+'/example.txt')
    data_PA = data_PA[np.logical_and(data_PA[:,0]>min(data_ZHS[:,5]), data_PA[:,0]<max(data_ZHS[:,5]))]
    
    dt_ZHS = (data_ZHS[1,5] - data_ZHS[0,5])*1e-9
    freq_ZHS = np.fft.rfftfreq(len(data_ZHS[:, 8]), dt_ZHS)
    
    fft_ZHS_x = np.fft.rfft(abs(data_ZHS[:, 7]*1e9))*dt_ZHS
    fft_ZHS_y = np.fft.rfft(abs(data_ZHS[:, 8]*1e9))*dt_ZHS
    fft_ZHS_z = np.fft.rfft(abs(data_ZHS[:, 9]*1e9))*dt_ZHS
    
    dt_PA = (data_PA[1,0] - data_PA[0,0])*1e-9
    freq_PA = np.fft.rfftfreq(len(data_PA[:, 0]), dt_PA)
    
    fft_PA_x = np.fft.rfft(a*data_PA[:, 1])*dt_PA
    fft_PA_y = np.fft.rfft(a*data_PA[:, 2])*dt_PA
    fft_PA_z = np.fft.rfft(a*data_PA[:, 3])*dt_PA
    
    plt.plot(freq_ZHS, abs(fft_ZHS_y), color = 'blue', label = 'Ay')
    plt.plot(freq_PA, abs(fft_PA_y), color = 'blue', linestyle = ":")

    plt.plot(np.arange(10), 100*np.ones(10), color = 'black', linestyle = "-", label = 'ZHAireS')
    plt.plot(np.arange(10), 100*np.ones(10), color = 'black', linestyle = ":", label = 'P&A')
    plt.xlabel('Frequency (Hz)')
    plt.ylabel('Vector Potential Magnitude '+r'$(\mathrm{V} \, \mathrm{ns} \, \mathrm{m}^{-1} \mathrm{Hz}^{-1})$')
    plt.grid(which = 'both')
    plt.legend()
    plt.xscale('log')
    plt.yscale('log')

def plot_comp(dir):
    
    data_ZHS = np.loadtxt(dir+'/timefresnel-root.dat', skiprows = 10)
    data_PA = np.loadtxt(dir+'/example.txt')
    data_PA = data_PA[np.logical_and(data_PA[:,0]>min(data_ZHS[:,5]), data_PA[:,0]<max(data_ZHS[:,5]))]
    
    fig = plt.figure()
    
    plt.plot(data_ZHS[:,5], data_ZHS[:,7]*1e9, color = 'red', label = 'Vx')
    plt.plot(data_ZHS[:,5], data_ZHS[:,8]*1e9, color = 'blue', label = 'Vy')
    plt.plot(data_ZHS[:,5], data_ZHS[:,9]*1e9, color = 'green', label =' Vz' )
    plt.plot(data_PA[:,0], a*data_PA[:,1], color = 'red', linestyle = ":")
    plt.plot(data_PA[:,0], a*data_PA[:,2], color = 'blue', linestyle = ":")
    plt.plot(data_PA[:,0], a*data_PA[:,3], color = 'green', linestyle = ":")
    plt.ylim(1.2*np.min(np.concatenate([a*data_PA[:,1], a*data_PA[:,2], a*data_PA[:,3], data_ZHS[:,7]*1e9, data_ZHS[:,8]*1e9, data_ZHS[:,9]*1e9])), 1.2*np.max(np.concatenate([a*data_PA[:,1], a*data_PA[:,2], a*data_PA[:,3], data_ZHS[:,7]*1e9, data_ZHS[:,8]*1e9, data_ZHS[:,9]*1e9])))
    plt.plot(np.arange(10), -100*np.ones(10), color = 'black', linestyle = "-", label = 'ZHAireS')
    plt.plot(np.arange(10), -100*np.ones(10), color = 'black', linestyle = ":", label = 'P&A')
    plt.grid(which = 'both')
    plt.xlabel('Time (ns)')
    plt.ylabel('A (V/m)')
    plt.legend()
    
    ########################################################################################################
    
    dt_ZHS = (data_ZHS[1,5] - data_ZHS[0,5])*1e-9
    freq_ZHS = np.fft.rfftfreq(len(data_ZHS[:, 8]), dt_ZHS)
    
    fft_ZHS_x = np.fft.rfft(abs(data_ZHS[:, 7]*1e9))*dt_ZHS
    fft_ZHS_y = np.fft.rfft(abs(data_ZHS[:, 8]*1e9))*dt_ZHS
    fft_ZHS_z = np.fft.rfft(abs(data_ZHS[:, 9]*1e9))*dt_ZHS
    
    dt_PA = (data_PA[1,0] - data_PA[0,0])*1e-9
    freq_PA = np.fft.rfftfreq(len(data_PA[:, 0]), dt_PA)
    
    fft_PA_x = np.fft.rfft(a*data_PA[:, 1])*dt_PA
    fft_PA_y = np.fft.rfft(a*data_PA[:, 2])*dt_PA
    fft_PA_z = np.fft.rfft(a*data_PA[:, 3])*dt_PA
    
    fig = plt.figure()
    
    plt.plot(freq_ZHS, abs(fft_ZHS_x), color = 'red', label = 'Ax')
    plt.plot(freq_ZHS, abs(fft_ZHS_y), color = 'blue', label = 'Ay')
    plt.plot(freq_ZHS, abs(fft_ZHS_z), color = 'green', label = 'Az')
    plt.plot(freq_PA, abs(fft_PA_x), color = 'red', linestyle = ":")
    plt.plot(freq_PA, abs(fft_PA_y), color = 'blue', linestyle = ":")
    plt.plot(freq_PA, abs(fft_PA_z), color = 'green', linestyle = ":")

    plt.plot(np.arange(10), 100*np.ones(10), color = 'black', linestyle = "-", label = 'ZHAireS')
    plt.plot(np.arange(10), 100*np.ones(10), color = 'black', linestyle = ":", label = 'P&A')
    plt.xlabel('Frequency (Hz)')
    plt.ylabel('Vector Potential Magnitude '+r'$(\mathrm{V} \, \mathrm{ns} \, \mathrm{m}^{-1} \mathrm{Hz}^{-1})$')
    plt.grid(which = 'both')
    plt.legend()
    plt.xscale('log')
    plt.yscale('log')

    
    PA_dxdt = -np.diff(data_PA[:,1]*a)/np.diff(data_PA[:,0])
    PA_dydt = -np.diff(data_PA[:,2]*a)/np.diff(data_PA[:,0])
    PA_dzdt = -np.diff(data_PA[:,3]*a)/np.diff(data_PA[:,0])
    
    ZHS_dxdt = -np.gradient(data_ZHS[:,7]*1e9, data_ZHS[1,5]-data_ZHS[0,5])
    ZHS_dydt = -np.gradient(data_ZHS[:,8]*1e9, data_ZHS[1,5]-data_ZHS[0,5])
    ZHS_dzdt = -np.gradient(data_ZHS[:,9]*1e9, data_ZHS[1,5]-data_ZHS[0,5])
    
    fig = plt.figure()
    plt.plot(data_PA[:-1,0], PA_dzdt, label = 'P&A')
    plt.plot(data_ZHS[:,5], data_ZHS[:,13], label = 'ZHAireS')
    plt.xlabel('Time (ns)')
    plt.ylabel('E (V/m)')
    plt.grid(which = 'both')
    # plt.xlim(-1700, -1400)
    # plt.ylim(-0.00045, 0.00015)

    plt.legend()
    
    return 1

def intensity_plot(dir):
    os.chdir(dir)
    files = os.listdir()
    xs = []
    ys = []
    strength_ZHS = []
    strength_PA = []
    for i in range(0, len(files)):
        print(i)
        #i = np.random.randint(0,1600)
        #file = 'test'
        data_ZHS = np.loadtxt(files[i]+'/timefresnel-root.dat', skiprows = 10)
        data_PA = np.loadtxt(files[i]+'/example.txt')
        


        PA_dxdt = -np.gradient(data_PA[:,1]*a, (data_PA[-1,0]-data_PA[0,0])/len(data_PA[:,0]))
        PA_dydt = -np.gradient(data_PA[:,2]*a, (data_PA[-1,0]-data_PA[0,0])/len(data_PA[:,0]))
        PA_dzdt = -np.gradient(data_PA[:,3]*a, (data_PA[-1,0]-data_PA[0,0])/len(data_PA[:,0]))
        
        ZHS_dxdt = -np.gradient(data_ZHS[:,7]*1e9, data_ZHS[1,5]-data_ZHS[0,5])
        ZHS_dydt = -np.gradient(data_ZHS[:,8]*1e9, data_ZHS[1,5]-data_ZHS[0,5])
        ZHS_dzdt = -np.gradient(data_ZHS[:,9]*1e9, data_ZHS[1,5]-data_ZHS[0,5])
        
        string_data = files[i].split('_')
        xs.append(int(string_data[0])-1000)
        ys.append(int(string_data[1])-1000)
        
        # ZHS_int = np.trapz((data_ZHS[:,7]**2+data_ZHS[:,8]**2+data_ZHS[:,9]**2)*1e18, data_ZHS[:,5])
        # PA_int = np.trapz((data_PA[:,1]**2+data_PA[:,2]**2+data_PA[:,3]**2)*a**2, data_PA[:,0])
        # 
        # ZHS_int = np.trapz((ZHS_dxdt**2+ZHS_dydt**2+ZHS_dzdt**2), data_ZHS[:,5])
        # PA_int = np.trapz((PA_dxdt**2+PA_dydt**2+PA_dzdt**2), data_PA[:,0])
        
        ZHS_int = np.sum(ZHS_dxdt**2+ZHS_dydt**2+ZHS_dzdt**2)
        PA_int = np.sum(PA_dxdt**2+PA_dydt**2+PA_dzdt**2)
        
        #*(data_ZHS[1,5]-data_ZHS[0,5])
        #*(data_PA[1,0]-data_PA[0,0])
        
        strength_ZHS.append(ZHS_int)
        strength_PA.append(PA_int)

    xs = np.array(xs)
    ys = np.array(ys)
    strength_ZHS = np.array(strength_ZHS)
    strength_PA = np.array(strength_PA)
    
    strength_ZHS[np.logical_and(xs == -5, ys == -10)] = 0
    strength_PA[np.logical_and(xs == -5, ys == -10)] = 0
    
    unique_xs = np.unique(xs)
    unique_ys = np.unique(ys)
    
    # ZHS = []
    # PA = []
    # 
    # for i in range(0, len(unique_xs)):
    #     ZHS_ints = []
    #     PA_ints = []
    #     for j in range(0, len(unique_ys)):
    #         ZHS_ints.append(strength_ZHS[np.logical_and(xs == unique_xs[i], ys == unique_ys[j])])
    #         PA_ints.append(strength_PA[np.logical_and(xs == unique_xs[i], ys == unique_ys[j])])
    #     ZHS.append(ZHS_ints)
    #     PA.append(PA_ints)
    #     
    # ZHS = np.array(ZHS)[:,:,0]
    # PA = np.array(PA)[:,:,0]
    
    #xs, ys = np.meshgrid(unique_xs, unique_ys)
    # ZHS[np.logical_and(xs == 0, ys == 0)] = 0
    # PA[np.logical_and(xs == 0, ys == 0)] = 0
    
    grid_x, grid_y = np.meshgrid(np.linspace(xs.min(), xs.max(), 200), np.linspace(ys.min(), ys.max(), 200))
    grid_ZHS = griddata((xs, ys), strength_ZHS, (grid_x, grid_y), method='cubic')
    grid_PA = griddata((xs, ys), strength_PA, (grid_x, grid_y), method='cubic')

    fig = plt.figure()
    # Create a 2D plot with color representation of z values
    plt.pcolormesh(grid_x, grid_y, grid_ZHS, cmap='viridis')
    plt.colorbar()

    # Set labels and title
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title('ZHS')
    
    fig = plt.figure()
    # Create a 2D plot with color representation of z values
    plt.pcolormesh(grid_x, grid_y, grid_PA, cmap='viridis')
    plt.colorbar()

    # Set labels and title
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title('PA')

    
#intensity_plot('3thinning')
plot_comp('test')
# os.chdir('3thinning')
# files = os.listdir()
# for i in range(0, 10):
#     plot_comp(files[i])

plt.show()
