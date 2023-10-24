#include <iostream>
#include <fstream>

#include <iomanip>
#include <vector>
#include <tuple>
#include <unordered_map>
#include <algorithm>
#include <cmath>
#include <cfloat>

#include <random>

#include <cstdint>
#include <cstring>

#include <chrono>
#include <thread>

//double bs;
//double ts;

////////////////////////////////////////////////////////////////////////////

//Constants
constexpr double c = 299792458e-9; // nmeters/s
constexpr double m_e = 0.511; // electron mass in MeV
constexpr double max_rel_ang = 1.1547; // this is 1./sin(60. * M_PI/180.)

double ant_x;
double ant_y;
double ant_z;

// custom equality function for std::array<double, 4>
struct ArrayEqual {
    bool operator()(const std::array<double, 4>& arr1, const std::array<double, 4>& arr2) const {
        return arr1 == arr2;
    }
};

/*struct ArrayHash{
    std::size_t operator()(const std::array<double, 4>& arr) const {
    std::size_t hash = 5315; //65599
    for (int i=0; i<4; i++){
        uint8_t *bytePtr = (uint8_t *)&arr[i];
        for(int j=0; j<8; j++){
            uint8_t c = bytePtr[j];
            hash = ((hash << 5) + hash) + c;
            //hash = hash *101+c;
            }
    }
    return hash;
    }     
};
*/

struct ArrayHash {
    std::size_t operator()(const std::array<double, 4>& arr) const {
//    std::cout << "blah" <<std::endl;
        // the hash for a given x, y, z, t will be incremented every time the hash
        // function is called. For a 32/64 bit system: 4e9/2e19 unique hashes

        std::size_t h1 = std::hash<double>()(arr[0]);
        std::size_t h2 = std::hash<double>()(arr[1]);
        std::size_t h3 = std::hash<double>()(arr[2]);
        std::size_t h4 = std::hash<double>()(arr[3]);

        return h1 ^ (h2 << 1) ^ (h3 << 2) ^ (h4 << 3);

    }
};

std::unordered_map<std::array<double, 4>, std::array<double, 18>, ArrayHash, ArrayEqual> grid;
//std::unordered_map<std::array<double, 4>, std::array<double, 13>, ArrayHash, ArrayEqual> grid;

int r = 0;

////////////////////////////////////////////////////////////////////////////

//Simulation Parameters
//double max_freq = 8e7; //Hz
constexpr double max_freq = 1000e6;
constexpr double wavelen = c*1e9/max_freq; //m
constexpr double scale = 0.5;  //Scale of Fraunhofer limit to consider for minimum box size

constexpr double zenith = 70*M_PI/180.;
constexpr double azimuth = 0*M_PI/180.;
constexpr std::array<double, 3> dir = {sin(zenith)*cos(azimuth), sin(zenith)*sin(azimuth), cos(zenith)};

double loading_time = 0;
double checking_time = 0;
double division_time = 0;
double cell_t_time = 0;
double dist_time = 0;
double insert_time = 0;
////////////////////////////////////////////////////////////////////////////

//Auxillary functions

//Cross product
std::array<double, 3> cross_product(std::array<double, 3> a, std::array<double, 3> b){
    return {a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]};
}

//Magnitude of vector
double magnitude(std::array<double, 3> a){
    return sqrt( pow(a[0], 2) + pow(a[1], 2) + pow(a[2],2));
}

//Distance from point to line
double point_distance(std::array<double, 3> p){
    //double r_max = 10000000; //m potentially change to max alt in ZHAireS
    //std::array<double, 3> x2 = {r_max*sin(zenith)*cos(azimuth), r_max*sin(zenith)*sin(azimuth), r_max*cos(zenith)};
    return magnitude(cross_product(dir,p));
}

//Finds the time to the nearest cell edge in units of box size
double get_diff(double cx, double x, double vx, double box_size) {

    return (vx < 0) ? fabs((box_size/2.+x-cx)/vx) : fabs((box_size/2.-x+cx)/vx);
}


//Finds the nearest grid cell edge (in time)
double find_dt(double x, double y, double z, double t, double vx, double vy, double vz, std::array<double, 4> cell_id, double t2, double box_size, double time_step) {

    double tdiff = fabs(std::floor(t/time_step)+1. - t/time_step)*time_step;
    tdiff = std::min(tdiff, get_diff(cell_id[0], x, vx, box_size));
    tdiff = std::min(tdiff, get_diff(cell_id[1], y, vy, box_size));
    tdiff = std::min(tdiff, get_diff(cell_id[2], z, vz, box_size));

    if ((tdiff + t) > t2) tdiff = t2-t;
    
    //Small offset to account for interbox behavior
    return tdiff+1e-2;

}

double limit_Fr(double cell_x, double cell_y, double cell_z, 
        double antx, double anty, double antz, double wavelen, double scale) 
{
    //Get minimum antenna distance from cell centerpoint

    
    double R = DBL_MAX;
    R = std::min(R, sqrt( pow(cell_x - antx, 2) + pow(cell_y- anty, 2) + pow(cell_z - antz,2)));
    
    //Calculate scaled Fraunhofer limit from cell centerpoint to closest antenna (WHY 6PI)
    double fr_size = scale*fabs(max_rel_ang * sqrt(R * wavelen/(6*M_PI)));
    
    return fr_size;
}

//Check whether box size obeys Fraunhofer diffraction condition for maximum frequency considered and closest antenna
double check_size(std::array<double, 4> cell_id, 
        double antx, double anty, double antz,
        double box_size, double wavelen, double scale) 
{
    //Get minimum antenna distance from cell centerpoint
    using namespace std::chrono;
    auto start = high_resolution_clock::now();

    
    double R = DBL_MAX;
    R = std::min(R, sqrt( pow(cell_id[0] - antx, 2) + pow(cell_id[1]- anty, 2) + pow(cell_id[2] - antz,2)));
    //for(std::vector<double>::size_type i = 0; i < antx.size(); i++) {
    //    double tempR = sqrt( pow(cell_id[0] - antx[i], 2) + pow(cell_id[1]- anty[i], 2) + pow(cell_id[2] - antz[i],2));
    //    R = std::min(R, tempR);
    //}
    
    //Calculate scaled Fraunhofer limit from cell centerpoint to closest antenna (WHY 6PI)
    double fr_box_size = scale*fabs(max_rel_ang * sqrt(R * wavelen/(6*M_PI)));
    
    auto end = high_resolution_clock::now();
    duration<double> time_span = duration_cast<duration<double>>(end - start);
    checking_time += time_span.count();
    
    return box_size > fr_box_size;
}

double get_n(double z){
    double Rs = 325.;
    double Kr = 0.1218;
    return 1. + Rs * exp(-Kr * z) * 1e-6;
}


double get_alt_from_xyz(double x, double y, double z) {
    double Re = 6371.;
    double zv = sqrt(pow(Re + z,2) + x*x + y*y) - Re;
    return zv;
}

double get_neff_austin(double cell_x, double cell_y, double cell_z, double ant_x, double ant_y, double ant_z){
    double Rs = 325.;
    double Kr = 0.1218;
    double theta = atan(sqrt(pow(cell_x-ant_x,2)+pow(cell_y-ant_y,2))/(cell_z-ant_z));
    
    double integral_at_cell = -(Rs/Kr)*exp(-Kr*cell_z*1e-3);
    double integral_at_ant = -(Rs/Kr)*exp(-Kr*ant_z*1e-3);
    return 1+fabs(1e-6*(integral_at_ant-integral_at_cell)/cos(theta))/(cell_z*1e-3);
}

double get_neff_ZHS(double cell_x, double cell_y, double cell_z, double ant_x, double ant_y, double ant_z){
    double Rs = 325.;
    double Kr = 0.1218;
    double avn = (Rs/(Kr*(cell_z*1e-3-ant_z*1e-3)))*(exp(-Kr*cell_z*1e-3)-exp(-Kr*ant_z*1e-3));
    return 1+fabs(1e-6*avn);
//         if(usevarn)then 
//            hd=(injz-zant(na))/1.d3 !detector altitude
//            h0=(injz-(z1+z2)/2.d0)/1.d3
//            if(dabs(hd-h0) .gt. 1.d-10)then
//               avn=(ns/(kr*(hd-h0)))*(exp(kr*hd)-exp(kr*h0))
//            else
//               avn=ns*exp(kr*h0)
//               print*,"Effective n: h0=hd"
//            endif
//            ref_n=1.d0+1.d-6*avn !average (effective) n
//            rh=ns*exp(kr*h0) !refractivity at emission
//            nh=1.d0+1.d-6*rh !n at emission
//         else 
//            nh=ref_n
//         endif
//c     ///////////
}

//def n_eff_austin_3D(pos_emit, pos_ant):
//    A = np.sqrt(pos_ant[0]**2+pos_ant[1]**2+(pos_ant[2]+Re)**2)
//    B = np.sqrt(pos_emit[0]**2+pos_emit[1]**2+(pos_emit[2]+Re)**2)
//    C = np.sqrt((pos_ant[0]-pos_emit[0])**2+(pos_ant[1]-pos_emit[1])**2+(pos_ant[2]-pos_emit[2])**2)
//    
//    theta = np.arccos((B**2-A**2-C**2)/(2*A*C))
//    a = Kr*np.cos(theta)
//    b = Kr*np.sin(theta)**2/(2*A)
//    const = np.exp(Kr*(Re-A))
//    
//    erf1_factor = a/(2*np.sqrt(b))
//    erf2_factor = (a+2*b*C)/(2*np.sqrt(b))    
//
//    if erf1_factor < 25:
//        term1 = np.sqrt(np.pi)*np.exp(a**2/(4*b))*(math.erfc(erf1_factor))/(2*np.sqrt(b))
//        term2 = np.sqrt(np.pi)*np.exp(a**2/(4*b))*(math.erfc(erf2_factor))/(2*np.sqrt(b))
//        return abs(const*(Rs/C)*(term2-term1))
//    else:
//        return (Rs/(C*Kr*np.cos(theta*np.pi/180)))*(1-np.exp(-Kr*C*np.cos(theta*np.pi/180)))

double get_neff_austin_3D(double cell_x, double cell_y, double cell_z, double ant_x, double ant_y, double ant_z){
    
    double Rs = 325.;
    double Kr = 0.1218;
    double Re = 6371;

    double A = sqrt(pow(ant_x*1e-3, 2)+pow(ant_y*1e-3, 2)+pow(ant_z*1e-3+Re, 2));
    double B = sqrt(pow(cell_x*1e-3, 2)+pow(cell_y*1e-3, 2)+pow(cell_z*1e-3+Re, 2));
    double C = sqrt(pow(ant_x-cell_x, 2)+pow(ant_y-cell_y, 2)+pow(ant_z-cell_z, 2))*1e-3;
    
    double theta = acos((pow(B, 2)-pow(A,2)-pow(C, 2))/(2*A*C));
    double a = Kr*cos(theta);
    double b = Kr*pow(sin(theta), 2)/(2*A);
    
    double exp_const = exp(Kr*(Re-A));
    double erf1_factor = a/(2*sqrt(b));
    double erf2_factor = (a+2*b*C)/(2*sqrt(b));
    
    double neff = 0.;
    
    if(erf1_factor < 25){
        double term1 = sqrt(M_PI)*exp(pow(a, 2)/(4*b))*(erfc(erf1_factor))/(2*sqrt(b));
        double term2 = sqrt(M_PI)*exp(pow(a, 2)/(4*b))*(erfc(erf2_factor))/(2*sqrt(b));
        neff += 1+1e-6*fabs(exp_const*(Rs/C)*(term2-term1));
    }
    else{
        neff += 1+1e-6*fabs(Rs/(C*Kr*cos(theta))*(exp(-Kr*cell_z*1e-3)-exp(-Kr*ant_z*1e-3)));
    }
    
    if (neff > 1.000325 || std::isnan(neff) || std::isinf(neff)){
        std::cout << "bad_neff!!!!111" << std::endl;
    }
    
    
    if (neff > 1.000325 || std::isnan(neff) || std::isinf(neff)){
        std::cout<< std::setprecision(15) << neff << std::endl;
        std::cout<< cell_x << std::endl;
        std::cout<< cell_y << std::endl;
        std::cout<< cell_z << std::endl;
        std::cout<< ant_x << std::endl;
        std::cout<< ant_y << std::endl;
        std::cout<< ant_z << std::endl;
        
    }
    
    //if (neff > 1.000325 || std::isnan(neff) || std::isinf(neff)) neff = 1.000325;
    return neff;
}

double get_neff(double cell_x, double cell_y, double cell_z, double ant_x, double ant_y, double ant_z, double theta, double zhs_z0) {
    // get effective index of refraction
    // split in approximation method is due to float precision stuff

    //this stuff is defined in km
    double R = 1e-3*magnitude({cell_x-ant_x, cell_y-ant_y, cell_z-ant_z});
    double R2 = R*R;
    double hDec = zhs_z0*1e-3 - get_alt_from_xyz(1e-3*cell_x, 1e-3*cell_y, 1e-3*cell_z);
    double hDet = zhs_z0*1e-3 - get_alt_from_xyz(1e-3*ant_x, 1e-3*ant_y, 1e-3*ant_z);
    double Re = 6371.;
    double Rs = 325.;
    double Kr = 0.1218;
    double ReDec = hDec + Re;
    double ReDec2 = ReDec*ReDec;
    double ReDet = hDet + Re;
    double ReDet2 = ReDet*ReDet;
    double thetaRel;
    if (hDec > hDet) {
        thetaRel = acos((ReDec2 - R2 - ReDet2)/(2*R*ReDet));
    } else {
        thetaRel = acos((ReDet2 - R2 - ReDec2)/(2*R*ReDec));
    }
    double sinthetaRel = sin(thetaRel);
    double costhetaRel = cos(thetaRel);
    double neff = 0.;
    if (theta > 40./180. * M_PI) {
        double expconst = -Kr*hDec + Kr*pow(costhetaRel,2) * ReDec/(2*pow(sinthetaRel,2));
        double integralConst = Rs/R * exp(expconst) * sqrt(M_PI * ReDec/(2*Kr))/sinthetaRel;
        double term1 = erfc(sqrt(Kr*ReDec/2.)/sinthetaRel * (costhetaRel + R * pow(sinthetaRel,2)/ReDec));
        double term2 = erfc(sqrt(Kr*ReDec/2.)/sinthetaRel * costhetaRel);
        double Reff = integralConst * (term2 - term1);
        neff += 1. + fabs(Reff)*1e-6;
    } else {
        double expconst = -Kr*hDec;
        double integralConst = Rs/R * exp(expconst) * sqrt(M_PI * ReDec/(2*Kr))/sinthetaRel;
        double term1 = exp(-Kr*ReDec/2. * (2*R/ReDec*costhetaRel + R2 * pow(sinthetaRel,2)/ReDec2)) / (sqrt(M_PI*Kr*ReDec/2.) * (costhetaRel/sinthetaRel + R * sinthetaRel/ReDec));
        double term2 = 1. / (sqrt(M_PI*Kr*ReDec/2.) * costhetaRel/sinthetaRel);
        double Reff = integralConst * (term2 - term1);
        neff += 1. + fabs(Reff)*1e-6;
    }
    // neff should never be greater than this value
    if (neff > 1.000325 || std::isnan(neff) || std::isinf(neff)){
        std::cout << "bad_neff" << std::endl;
    }
    if (neff > 1.000325 || std::isnan(neff) || std::isinf(neff)) neff = 1.000325;
    return neff;
    
}

////////////////////////////////////////////////////////////////////////////

extern "C" {

    void add_one(){
        size_t reserved_elements = 5e7;
        grid.reserve(reserved_elements);
    }

    void update_vector(int PID, double X1, double Y1, double Z1, double T1, double X2, double Y2, double Z2, double T2, double E1, double E2, double w, double ANTX, double ANTY, double ANTZ) {
        T1 /= c;
        T2 /= c;
        
        ant_x = ANTX;
        ant_y = ANTY;
        ant_z = ANTZ; //PLEASE DONT FORGET THAT THIS IS IN THE WEIRD ZHAIRES COORDINATES: ANTZ = 50KM-ANTZ
        
        //std::cout << ant_x <<std::endl;
        //std::cout << ant_y <<std::endl;
        //std::cout << ant_z <<std::endl;
        
        
        using namespace std::chrono;
        auto start = high_resolution_clock::now();
        
        double L = sqrt(pow(X2 - X1, 2) + pow(Y2 - Y1, 2) + pow(Z2 - Z1, 2));
        
        double Eave = (E1 + E2)/2.;
        double beta = c*sqrt(1. - 1./pow(Eave/m_e, 2));
        
        double betax = beta * (X2 - X1)/L;
        double betay = beta * (Y2 - Y1)/L;
        double betaz = beta * (Z2 - Z1)/L;
        
        auto end = high_resolution_clock::now();
        duration<double> time_span = duration_cast<duration<double>>(end - start);
        loading_time += time_span.count();
        
        //std::cout << "X1: " << X1 << std::endl;
        //std::cout << "X2: " << X2 << std::endl;
        //std::cout << "Y1: " << Y1 << std::endl;
        //std::cout << "Y2: " << Y2 << std::endl;
        //std::cout << "Z1: " << Z1 << std::endl;
        //std::cout << "Z2: " << Z2 << std::endl;
        //std::cout << "T1: " << T1 << std::endl;
        //std::cout << "T2: " << T2 << std::endl;
        //std::cout << "L: " << L << std::endl;
        //
        //double fraun = limit_Fr(X1, Y1, Z1, 100, 0, 50000-0, wavelen, scale);
        //std::cout << "Fraunhofer: " << fraun << std::endl;
        //
        //std::cout << "Splitting" << std::endl;
        //std::cout << "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" << std::endl;

        //Splitting the track into boxes
        while(T1 < T2) {
            //Initial Box Size
            double bs = 50.; //meters
            double ts = bs/c; //ns
            std::array<double, 4> cell_id = {std::floor(X1/bs)*bs + bs/2., std::floor(Y1/bs)*bs + bs/2., std::floor(Z1/bs)*bs + bs/2., std::floor(T1/ts)*ts + ts/2.};
            
            
            //If the box is too large, divide into 8 subcells
            while(check_size(cell_id, ant_x, ant_y, ant_z, bs, wavelen, scale)) {
                start = high_resolution_clock::now();
                bs /= 2.; 
                ts /= 2.;
                double xcp = cell_id[0]+bs/2.;
                double ycp = cell_id[1]+bs/2.;
                double zcp = cell_id[2]+bs/2.;
                
                double xcm = cell_id[0]-bs/2.;
                double ycm = cell_id[1]-bs/2.;
                double zcm = cell_id[2]-bs/2.;
                
                //double tcp = cell_id[3]+ts/2.;
                //double tcm = cell_id[3]-ts/2.;
                
                cell_id[0] = (fabs(X1-xcp) < fabs(X1-xcm)) ? xcp : xcm;
                cell_id[1] = (fabs(Y1-ycp) < fabs(Y1-ycm)) ? ycp : ycm;
                cell_id[2] = (fabs(Z1-zcp) < fabs(Z1-zcm)) ? zcp : zcm;
                cell_id[3] = std::floor(T1/ts)*ts + ts/2.;
                //cell_id[3] = (fabs(T1-tcp) < fabs(T1-tcm)) ? tcp : tcm;
                
                end = high_resolution_clock::now();
                time_span = duration_cast<duration<double>>(end - start);
                division_time += time_span.count();
            }
            

            
            start = high_resolution_clock::now();
            //Find the nearest time edge and the difference between the edge and the track start time
            double dt_ij = find_dt(X1, Y1, Z1, T1, betax, betay, betaz, cell_id, T2, bs, ts);
            
            //std::cout << "X1: " << X1 << std::endl;
            //std::cout << "Y1: " << Y1 << std::endl;
            //std::cout << "Z1: " << Z1 << std::endl;
            //std::cout << "T1: " << T1 << std::endl;
            //std::cout << "Cell_X: " << cell_id[0] << std::endl;
            //std::cout << "Cell_Y: " << cell_id[1] << std::endl;
            //std::cout << "Cell_Z: " << cell_id[2] << std::endl;
            //std::cout << "Cell_T: " << cell_id[3] << std::endl;
            //std::cout << "dT: " << dt_ij << std::endl;
            //std::cout << "wwwwwwwwwwwwwwwwwww" << std::endl;
            
            end = high_resolution_clock::now();
            time_span = duration_cast<duration<double>>(end - start);
            
            cell_t_time += time_span.count();
            
            start = high_resolution_clock::now();
            double dist = point_distance({X1, Y1, 50000-Z1});
            end = high_resolution_clock::now();
            time_span = duration_cast<duration<double>>(end - start);
            dist_time += time_span.count();
            //if(dist<40){
            int pid = PID/2;

            std::array<double, 18> data = {0};
            //std::array<double, 13> data = {0};
            data[0] = bs;
            //data[9] = DBL_MAX;
            
            start = high_resolution_clock::now();
            auto [it, grid_insert] = grid.insert({cell_id, data});

            
            
            std::ofstream outfile;
            outfile.open("example_tracks.txt", std::ios_base::app);
            //95656.25 6.25 33131.25 -323994.975217155064 //-4 thinning
            it->second[1] += 1; //number of tracks
            if(fabs(cell_id[0]-95656.25)<0.01){
                if(fabs(cell_id[1]-6.25)<0.01){
                    if(fabs(cell_id[2]-33131.25)<0.01){
                        if(fabs(cell_id[3]+323994.975217155064)<0.01){
                            outfile << std::fixed << std::setprecision(12) << X1 << " " << Y1 << " " << Z1 << " " << T1 << " " << betax << " " << betay << " " << betaz << " " << dt_ij << " " << pid << " " << w << "\n";
                            //std::ofstream myfile ("example_tracks.txt");
                            //if (myfile.is_open())
                            //    {
                            //        myfile << std::fixed << std::setprecision(12) << X1;
                            //        myfile.close();
                            //    } 
                            //std::cout<<Y1<<std::endl;
                            //std::cout<<Z1<<std::endl;
                            //std::cout<<T1<<std::endl;
                            //std::cout<<dt_ij<<std::endl;
                        }
                    }
                }
            }
            if(it->second[1]>2490){
                std::cout<<std::fixed << std::setprecision(12) <<cell_id[0]<<std::endl;
                std::cout<<std::fixed << std::setprecision(12) <<cell_id[1]<<std::endl;
                std::cout<<std::fixed << std::setprecision(12) <<cell_id[2]<<std::endl;
                std::cout<<std::fixed << std::setprecision(12) <<cell_id[3]<<std::endl;
                
                std::cout<<"ttttttttttttttttttttttttttttttttttttttttttttttttttttttttt"<<std::endl;
            }
            

            it->second[2] += w*pid*(betax/c)*dt_ij; //J (current)
            it->second[3] += w*pid*(betay/c)*dt_ij;
            it->second[4] += w*pid*(betaz/c)*dt_ij;

            it->second[5] += w*(betax/c); //V (velocity)
            it->second[6] += w*(betay/c);
            it->second[7] += w*(betaz/c);
            
            
            
            //it->second[8] += w*dt_ij;
            it->second[8] = dt_ij;
            //it->second[8] = std::min(it->second[8], dt_ij);
            //if(it->second[8] == 0){
            //    it->second[8] = DBL_MAX;
            //}
            
            
            double mid_T = (T1+T1+dt_ij)/2.;
            double mid_X = (X1+X1+betax*dt_ij)/2.;
            double mid_Y = (Y1+Y1+betay*dt_ij)/2.;
            double mid_Z = (Z1+Z1+betaz*dt_ij)/2.;
            
            //it->second[1] += 1; //number of tracks
            //it->second[2] += w*pid*betax*dt_ij/c; //J (current)
            //it->second[3] += w*pid*betay*dt_ij/c;
            //it->second[4] += w*pid*betaz*dt_ij/c;
            //it->second[5] += w*betax*dt_ij/c; //V (velocity)
            //it->second[6] += w*betay*dt_ij/c;
            //it->second[7] += w*betaz*dt_ij/c;
            //it->second[8] += w*pid*betax/c; //dt
            //it->second[9] += w*pid*betay/c;
            //it->second[10] += w*pid*betaz/c;
            //it->second[11] += w*dt_ij;
            //
            //if(it->second[1]>1){
            //    std::cout << it->first[3] << std::endl;
            //    std::cout << it->first[0] << std::endl;
            //    std::cout << it->first[1] << std::endl;
            //    std::cout << it->first[2] << std::endl;
            //    std::cout << it->second[0] << std::endl;
            //    std::cout << "ttttttttttttttttttt" << std::endl;
            //    std::cout << it->second[9] << std::endl;
            //    std::cout << it->second[10] << std::endl;
            //    std::cout << it->second[11] << std::endl;
            //    std::cout << it->second[12] << std::endl;
            //    std::cout << it->second[8] << std::endl;
            //    std::cout << it->second[17] << std::endl;
            //    std::cout << "xxxxxxxxxxxxxxxxxxxx" << std::endl;
            //    std::cout << mid_T << std::endl;
            //    std::cout << mid_X << std::endl;
            //    std::cout << mid_Y << std::endl;
            //    std::cout << mid_Z << std::endl;
            //    std::cout << dt_ij << std::endl;
            //    std::cout << w << std::endl;
            //    std::cout << "gggggggggggggggggggg" << std::endl;
            //    std::cout << "                    " << std::endl;
            //}
            
            //it->second[8] = std::max(it->second[8], dt_ij);
            
            it->second[17] += w;
            
            //it->second[9] += w*(mid_T);
            //it->second[10] += w*(mid_X);
            //it->second[11] += w*(mid_Y);
            //it->second[12] += w*(mid_Z);

            it->second[9] = mid_T;
            it->second[10] = mid_X;
            it->second[11] = mid_Y;
            it->second[12] = mid_Z;            
            

            ////welford on dt calculated
            //double delta_T = mid_T - it->second[9];
            //double delta_X = mid_X - it->second[10];
            //double delta_Y = mid_Y - it->second[11];
            //double delta_Z = mid_Z - it->second[12];
            //it->second[9] += delta_T*w/it->second[17];
            //it->second[10] += delta_X*w/it->second[17];
            //it->second[11] += delta_Y*w/it->second[17];
            //it->second[12] += delta_Z*w/it->second[17];
            ////
            //double delta2_T = mid_T - it->second[9];
            //double delta2_X = mid_X - it->second[10];
            //double delta2_Y = mid_Y - it->second[11];
            //double delta2_Z = mid_Z - it->second[12];
            //
            //it->second[13] += delta_T*delta2_T*w;
            //it->second[14] += delta_X*delta2_X*w;
            //it->second[15] += delta_Y*delta2_Y*w;
            //it->second[16] += delta_Z*delta2_Z*w;
            
            it->second[13] += w*pid*betax/c; //dt
            it->second[14] += w*pid*betay/c;
            it->second[15] += w*pid*betaz/c;
            
            end = high_resolution_clock::now();
            time_span = duration_cast<duration<double>>(end - start);
            
            insert_time += time_span.count();

            
            //}
            
            X1 += betax * dt_ij;
            Y1 += betay * dt_ij;
            Z1 += betaz * dt_ij;
            T1 += dt_ij;
            
            
            //std::this_thread::sleep_for(seconds(5));
        }
    //std::cout << "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" << std::endl;
    }
    
    void calculate_radio(){
        //std::ofstream myfile_2 ("example_cells.txt");
        //if (myfile_2.is_open()){
        //    for(auto it = grid.cbegin(); it != grid.cend(); ++it){
        //        myfile_2 << it->first[0] << "    " << it->first[1] << "    " << it->first[2] << "    " << it->first[3] << "    " << it->second[0] << "    " << it->second[1] << "\n";
        //    }
        //    //for(int count = 0; count < size; count ++){
        //    //    myfile << x[count] << " " ;
        //    //}
        //    myfile_2.close();
        //}
        
        constexpr double time_bound_ns = 10000;
        constexpr double sample_time_ns = 0.5;
        constexpr int num_samples = floor(2*time_bound_ns/sample_time_ns);
        
        std::array<double, num_samples> times;
        std::array<double, num_samples> VPx = {0};
        std::array<double, num_samples> VPy = {0};
        std::array<double, num_samples> VPz = {0};
        
        for(int i = 0; i<num_samples; ++i){
            times[i] = -time_bound_ns + i*sample_time_ns+0.25;
            
        }

        // loop over all cells
        for(auto it = grid.cbegin(); it != grid.cend(); ++it){
            
            //double cell_x = it->first[0];
            //double cell_y = it->first[1];
            //double cell_z = it->first[2];
            //double cell_t = it->first[3];
            
            double cell_len = it->second[0];
            int n_in_cell = it->second[1];
            //std::cout << n_in_cell << std::endl;

            double Jx = it->second[2];
            double Jy = it->second[3];
            double Jz = it->second[4];
            double Vx = it->second[5];
            double Vy = it->second[6];
            double Vz = it->second[7];
            double dt = it->second[8];
            
            double cell_t = it->second[9];
            double cell_x = it->second[10];
            double cell_y = it->second[11];
            double cell_z = it->second[12];
            
            double N = it->second[17];
            
            double delta_t = sqrt(it->second[13]/N);
            double delta_x = sqrt(it->second[14]/N);
            double delta_y = sqrt(it->second[15]/N);
            double delta_z = sqrt(it->second[16]/N);
            
            double dtbotx = it->second[13];
            double dtboty = it->second[14];
            double dtbotz = it->second[15];
            //double dt = sqrt((pow(Jx, 2) + pow(Jy, 2) + pow(Jz,2))/(pow(dtbotx, 2) + pow(dtboty, 2) + pow(dtbotz,2)));
            
            //if(n_in_cell>1){
            //std::cout << n_in_cell << std::endl;
            //std::cout << cell_t << std::endl;
            //std::cout << delta_t << std::endl;
            //std::cout << "gggggggggggggggggggggggggggggggg" << std::endl;
            //}
            
            //std::default_random_engine generator;
            //
            //std::normal_distribution<double> t_distribution(cell_t,delta_t);
            //std::normal_distribution<double> x_distribution(cell_x,delta_x);
            //std::normal_distribution<double> y_distribution(cell_y,delta_y);
            //std::normal_distribution<double> z_distribution(cell_z,delta_z);
            //
            //cell_t = t_distribution(generator);
            //cell_x = x_distribution(generator);
            //cell_y = y_distribution(generator);
            //cell_z = z_distribution(generator);
            
            //cell_t += 0.5*delta_t*((rand() - RAND_MAX/2.)/RAND_MAX);
            //cell_x += 0.5*delta_x*((rand() - RAND_MAX/2.)/RAND_MAX);
            //cell_y += 0.5*delta_y*((rand() - RAND_MAX/2.)/RAND_MAX);
            //cell_z += 0.5*delta_z*((rand() - RAND_MAX/2.)/RAND_MAX);
            
            //dt/=n_in_cell;
            //double M2_dt = it->second[9];
            
            //double dtbotx = it->second[8];
            //double dtboty = it->second[9];
            //double dtbotz = it->second[10];
            
            
            //cell_t /= N;
            //cell_x /= N;
            //cell_y /= N;
            //cell_z /= N;
            
            //if(n_in_cell > 1){            
            //
            //
            //cell_x += 0.1*cell_len*((rand() - RAND_MAX/2.)/RAND_MAX);
            //cell_y += 0.1*cell_len*((rand() - RAND_MAX/2.)/RAND_MAX);
            //cell_z += 0.1*cell_len*((rand() - RAND_MAX/2.)/RAND_MAX);
            //
            //}
            
            //dt /= N;

            
            //if(M2_dt<0){
            //    M2_dt = 0;
            ////std::cout<< n_in_cell << std::endl;
            ////std::cout<< dt << std::endl;
            ////std::cout<< M2_dt << std::endl;
            ////std::cout<< sqrt(M2_dt/N) << std::endl;
            ////std::cout<< "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" << std::endl;
            //}
            
            //double dt_offset = sqrt(M2_dt/N);
            //dt += 2*dt_offset*((rand() - RAND_MAX/2.)/RAND_MAX);
	    
            //double dt = magnitude({Jx, Jy, Jz})/magnitude({dtbotx, dtboty, dtbotz})
	    //remember that my Jx has c... could mess things up
            //double dt = sqrt((pow(Jx, 2) + pow(Jy, 2) + pow(Jz,2))/(pow(dtbotx, 2) + pow(dtboty, 2) + pow(dtbotz,2)));
            //double dt = sqrt(dt_squared/N);
            //double dt = dt_squared/N;
            //double dt2 = cell_len/c;
            //double dt = cell_len/c;
            //dt = std::max(dt, dt2);
            
            //std::cout << dt/dt2 << std::endl;
            //double dt = it->second[12];

            Vx /= N;
            Vy /= N;
            Vz /= N;
            
            Jx /= dt;
            Jy /= dt;
            Jz /= dt;
            
            //double Jtot = sqrt(pow(Jx, 2)+pow(Jy, 2)+pow(Jz, 2));
            //
            //double zenith_J = acos(Jz/Jtot);
            //double azimuth_J = atan2(Jy, Jx);
            //
            //
            //double offset = 10*M_PI/180.;
            //
            //zenith_J += offset*((rand() - RAND_MAX/2.)/RAND_MAX);
            //azimuth_J += offset*((rand() - RAND_MAX/2.)/RAND_MAX);
            //
            ////if(n_in_cell < 2){
            ////zenith_J += offset;
            ////azimuth_J += offset;
            ////}
            //Jx = Jtot*sin(zenith_J)*cos(azimuth_J);
            //Jy = Jtot*sin(zenith_J)*sin(azimuth_J);
            //Jz = Jtot*cos(zenith_J);
            
            
            //will be an outer loop here for antennas
            //double t1 = cell_t - dt/2.;
            //double t2 = cell_t + dt/2.;
            double t1 = cell_t - dt/2;
            double t2 = cell_t + dt/2;
            double R = magnitude({cell_x-ant_x, cell_y-ant_y, cell_z-ant_z});
            std::array<double, 3> U = {(ant_x-cell_x)/R, (ant_y-cell_y)/R, (ant_z-cell_z)/R};
            double vpx = (pow(U[1], 2)+pow(U[2], 2))*Jx - U[0]*U[1]*Jy - U[0]*U[2]*Jz;
            double vpy = (pow(U[0], 2)+pow(U[2], 2))*Jy - U[0]*U[1]*Jx - U[1]*U[2]*Jz;
            double vpz = (pow(U[0], 2)+pow(U[1], 2))*Jz - U[0]*U[2]*Jx - U[1]*U[2]*Jy;
            
            double n = get_n(1e-3*(50000-cell_z));
            //double n = get_n(get_alt_from_xyz(1e-3*cell_x, 1e-3*cell_y, 1e-3*(50000-cell_z)));

            //double dtt = t2-t1; //the heck?
            //double n_eff = get_neff(cell_x, cell_y, cell_z, ant_x, ant_y, ant_z, zenith, 50000.);
            //double n_eff = get_neff(cell_x, cell_y, 50000-cell_z, ant_x, ant_y, 50000-ant_z, zenith, 50000.);
            //double n_eff = get_neff_austin_3D(cell_x, cell_y, 50000-cell_z, ant_x, ant_y, 50000-ant_z);
            
            
            double n_eff = get_neff_ZHS(cell_x, cell_y, 50000-cell_z, ant_x, ant_y, 50000-ant_z);
            //double n_eff = get_neff_austin(cell_x, cell_y, 50000-cell_z, ant_x, ant_y, 50000-ant_z);
            
            double auxb = n*(U[0]*Vx + U[1]*Vy + U[2]*Vz);
            double denom = 1.-auxb; //missing abs
            
            double tout1 = (n_eff*R)/c + t1 - auxb*(t1-cell_t)+0.25;
            double tout2 = (n_eff*R)/c + t2 - auxb*(t2-cell_t)+0.25;
            
            //std::cout << auxb << std::endl;
            //std::cout << denom << std::endl;
            //std::cout << tout1 << std::endl;
            //std::cout << tout2 << std::endl;  
            
            ///////////////////////////////////////////////////////////////////////////////
            //double Vtot = sqrt(pow(Vx, 2)+pow(Vy, 2)+pow(Vz, 2));
            //
            //Vx /= Vtot;
            //Vy /= Vtot;
            //Vz /= Vtot;
            //Vtot /= Vtot;
            //
            //double zenith_V = acos(Vz/Vtot);
            //double azimuth_V = atan2(Vy, Vx);
            //
            //
            //double offset = 45*M_PI/180.;
            //
            //zenith_V += offset*((rand() - RAND_MAX/2.)/RAND_MAX);
            //azimuth_V += offset*((rand() - RAND_MAX/2.)/RAND_MAX);
            //
            //Vx = Vtot*sin(zenith_V)*cos(azimuth_V);
            //Vy = Vtot*sin(zenith_V)*sin(azimuth_V);
            //Vz = Vtot*cos(zenith_V);
            //
            ///////////////////////////////////////////////////////////////////////////////
            //
            //auxb = n*(U[0]*Vx + U[1]*Vy + U[2]*Vz);
            //denom = fabs(1.-auxb);
            //
            //tout1 = (n_eff*R)/c + t1 - auxb*(t1-cell_t);
            //tout2 = (n_eff*R)/c + t2 - auxb*(t2-cell_t);
            
            //std::cout << auxb << std::endl;
            //std::cout << denom << std::endl;
            //std::cout << tout1 << std::endl;
            //std::cout << tout2 << std::endl;
            //
            //std::cout << "xxxxxxxxxxxxx" <<std::endl;
            
            double flip = (tout1<tout2) ? 1. : -1;
            double t_start = std::min(tout1, tout2);
            double t_end = std::max(tout1, tout2);
            
            int start_bin = floor((t_start - (times[0]))/sample_time_ns);
            int end_bin = floor((t_end - (times[0]))/sample_time_ns);


            double start_frac = ((times[0] + sample_time_ns * (start_bin+1)) - t_start)/sample_time_ns;
            double end_frac = (t_end - (times[0] + sample_time_ns * end_bin))/sample_time_ns;
            
            if(n_in_cell>0){
            
            if(end_bin <0){
                continue;
            }
            
            else if(start_bin > num_samples-1){
                continue;
            }
            
            else if(start_bin == end_bin){
                if(denom>1e-13){
                    VPx[start_bin] += fabs((t_start-t_end)/sample_time_ns)*2*vpx/R/denom; //no flip??
                    VPy[start_bin] += fabs((t_start-t_end)/sample_time_ns)*2*vpy/R/denom;
                    VPz[start_bin] += fabs((t_start-t_end)/sample_time_ns)*2*vpz/R/denom;
                    
                    //VPx[start_bin] += fabs(dt/sample_time_ns)*flip*2*vpx/R;
                    //VPy[start_bin] += fabs(dt/sample_time_ns)*flip*2*vpy/R;
                    //VPz[start_bin] += fabs(dt/sample_time_ns)*flip*2*vpz/R;
                    
                }
                else{                  
                    VPx[start_bin] += fabs(dt/sample_time_ns)*2*vpx/R;
                    VPy[start_bin] += fabs(dt/sample_time_ns)*2*vpy/R;
                    VPz[start_bin] += fabs(dt/sample_time_ns)*2*vpz/R;
                }
            }
            
            else{
                if(start_bin > -1){
                    VPx[start_bin] += start_frac*flip*2*vpx/R/denom;
                    VPy[start_bin] += start_frac*flip*2*vpy/R/denom;
                    VPz[start_bin] += start_frac*flip*2*vpz/R/denom;
                }
                if(end_bin < num_samples-1){
                    VPx[end_bin] += end_frac*flip*2*vpx/R/denom;
                    VPy[end_bin] += end_frac*flip*2*vpy/R/denom;
                    VPz[end_bin] += end_frac*flip*2*vpz/R/denom;
                }
                
                for(int i = std::max(start_bin+1, 1); i< std::min(end_bin, num_samples); ++i){
                    VPx[i] += flip*2*vpx/R/denom;
                    VPy[i] += flip*2*vpy/R/denom;
                    VPz[i] += flip*2*vpz/R/denom;
                }   
            }
            
            }
            //for(int i = 0; i < num_samples; ++i){
            //    if(abs(VPx[i])>0){
            //        std::cout << i << " " << times[i] << " " << VPx[i] <<"\n";
            //    }
            //}
            //std::cout<<"ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"<<std::endl;
        
            //std::cout << "Cell_X: " << cell_x << std::endl;
            //std::cout << "Cell_Y: " << cell_y << std::endl;
            //std::cout << "Cell_Z: " << cell_z << std::endl;
            //std::cout << "cell_t: " << cell_t << std::endl;
            //
            //std::cout << "dt: " << dt << std::endl;
            //std::cout << "dt (observer): " << t_end-t_start << std::endl;
            //std::cout << "start_bin: " << start_bin << std::endl;
            //std::cout << "end_bin: " << end_bin << std::endl;
            //std::cout << "start_frac: " << start_frac << std::endl;
            //std::cout << "end_frac: " << end_frac << std::endl;
            //std::cout << "Jx: " << flip*2*vpx/R/denom << std::endl;
            //
            //
            //
            //
            //
            //for(int i = 0; i < num_samples; ++i){
            //    if(125<times[i] && times[i]<180){
            //            std::cout << i << " " << times[i] << " " << VPx[i] <<"\n";
            //    }
            //}

            //std::cout <<"ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"<<std::endl;
        
        }
        
        std::ofstream myfile ("example.txt");
        if (myfile.is_open()){
            for(int i = 0; i < num_samples; ++i){
                myfile << times[i] << "    " << VPx[i] << "    " << VPy[i] << "    " << VPz[i] <<"\n";
            }
            //for(int count = 0; count < size; count ++){
            //    myfile << x[count] << " " ;
            //}
            myfile.close();
        }
  
    }
    
    void grid_statistics() {
        double small_box = DBL_MAX;
        double big_box = 0;
        double max_vals = 0;
        std::vector<double>::iterator index;
        int num_boxes = grid.size();
        double avg_box = 0;
        std::vector<double> box_sizes = {};
        std::vector<double> box_nums = {};
        
//        for(auto it = grid.cbegin(); it != grid.cend(); ++it)
//{
//    std::cout << it->first[0] << " " << it->first[1] << " " << it->first[2] << " " << it->first[3] << " " << it->second[0] << " " << it->second[1] << "\n";
//}

        box_sizes.push_back(grid.begin()->second[0]);
        box_nums.push_back(0);
        
        for( auto it = grid.begin(); it!=grid.end(); ) {
                    
            index = std::find (box_sizes.begin(), box_sizes.end(), it->second[0]);
            if (index != box_sizes.end()){
                box_nums[index-box_sizes.begin()] += 1;
                
            }
            else{
                box_sizes.push_back(it->second[0]);
                box_nums.push_back(0);  
            }
        
            avg_box += it->second[1];
            small_box = std::min(small_box, it->second[0]);
            big_box = std::max(big_box, it->second[0]);
            max_vals = std::max(max_vals, it->second[1]);
        
            grid.erase(it++);
        
        }
        
        
        std::cout << "--------------------------------------" << std::endl;
        std::cout << "Box Breakdown" << std::endl;
        std::cout<< "Number of boxes: " << num_boxes << std::endl;
        
        for(int j = 0; j<box_sizes.size(); j++){
            std::cout << "//////////////////" << std::endl;
            std::cout << "Side length: " << box_sizes[j] << std::endl;
            std::cout << "Number of boxes with side length: " << box_nums[j] << std::endl;
            std::cout << "//////////////////" << std::endl;
        }
        
        std::cout<< "Maximum number of tracks/box: " << max_vals <<std::endl;
        std::cout<< "Average number of tracks per box: " << avg_box/num_boxes << std::endl;
        std::cout << "--------------------------------------" << std::endl;
        std::cout << loading_time << std::endl;
        std::cout << checking_time << std::endl;
        std::cout << division_time << std::endl;
        std::cout << cell_t_time << std::endl;
        std::cout << dist_time << std::endl;
        std::cout << insert_time << std::endl;
    
    }
}
