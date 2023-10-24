#include <iostream>
#include <vector>
#include <tuple>
#include <unordered_map>
#include <algorithm>
#include <cmath>
#include <cfloat>

#include <cstdint>
#include <cstring>

#include <chrono>
#include <thread>

//double bs;
//double ts;

////////////////////////////////////////////////////////////////////////////

//Constants
constexpr double c = 299792458; // meters/s
constexpr double m_e = 0.511; // electron mass in MeV
constexpr double max_rel_ang = 1.1547; // this is 1./sin(60. * M_PI/180.)

// custom equality function for std::array<double, 4>
struct ArrayEqual {
    bool operator()(const std::array<double, 4>& arr1, const std::array<double, 4>& arr2) const {
        return arr1 == arr2;
    }
};

size_t q = 0;
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
//        return q;
    }
};

std::unordered_map<std::array<double, 4>, std::array<double, 2>, ArrayHash, ArrayEqual> grid;

////////////////////////////////////////////////////////////////////////////

//Simulation Parameters
//double max_freq = 8e7; //Hz
double max_freq = 1e9;
double wavelen = c/max_freq; //m
double scale = 0.1;  //Scale of Fraunhofer limit to consider for minimum box size

//Antenna Positions
std::vector<double> antx = {0.};
std::vector<double> anty = {0.};
std::vector<double> antz = {0.};

////////////////////////////////////////////////////////////////////////////

//Auxillary functions

//Cross product (potentiall go to 4D?)
std::vector<double> cross_product(std::vector<double> a, std::vector<double> b){
    return {a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]};
}

//Magnitude of vector
double magnitude(std::vector<double> a){
    return sqrt( pow(a[0], 2) + pow(a[1], 2) + pow(a[2],2));
}

//Distance from point to line
double point_distance(double zenith, double azimuth, std::vector<double> p){
    double r_max = 10000000; //m potentially change to max alt in ZHAireS
    std::vector<double> x2 = {r_max*sin(zenith)*cos(azimuth), r_max*sin(zenith)*sin(azimuth), r_max*cos(zenith)};
    return magnitude(cross_product(x2,p))/magnitude(x2);
}

//Finds the time to the nearest cell edge in units of box size
double get_diff(double cx, double x, double vx, double box_size) {
    
    double diff;
    double smallest_diff = 1e-8;
    //std::cout.precision(17);
    //std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
    //std::cout << x << std::endl;
    //std::cout << vx << std::endl;
    //std::cout << cx << std::endl;
    x /= box_size;
    cx /= box_size;
    if (vx < 0) {
        diff = x-cx+0.5;
    } else {
        diff = cx+0.5-x;
    }
    // I dont understand this line
    // Machine precision maybe
    // Jumps to next box
    if (diff < smallest_diff) {
        //std::cout << "blargleblargleblargleblargleblargleblargleblargleblargleblargleblargle" << std::endl;
        //using namespace std::this_thread; // sleep_for, sleep_until
        //using namespace std::chrono; // nanoseconds, system_clock, seconds

        //sleep_for(seconds(1));
        if (vx < 0) {
            diff -= 1.;
        } else {
            diff += 1.;
        }
    }
    //return fabs(diff/(vx*c)*box_size);
    return fabs(diff/(vx*c*1e-9)*box_size);
}

//Finds the nearest grid cell edge (in time)
double find_t2(double x, double y, double z, double t, double vx, double vy, double vz, std::array<double, 4> cell_id, double t2, double box_size, double time_step) {

    double tdiff = get_diff(cell_id[0], x, vx, box_size);
    tdiff = std::min(tdiff, get_diff(cell_id[1], y, vy, box_size));
    tdiff = std::min(tdiff, get_diff(cell_id[2], z, vz, box_size));
    // I dont understand these lines
    double time_diff = fabs(std::floor(t/time_step)+1. - t/time_step)*time_step;
    if(time_diff < 1e-8) {
        std::cout << "heebleheebleheebleheebleheebleheebleheebleheeble" << std::endl;
        time_diff = fabs(std::floor(t/time_step)+2. - t/time_step)*time_step;
    }
    tdiff = std::min(tdiff, time_diff);
    double t2_em = t + tdiff;
    if (t2_em > t2) t2_em = t2;
    return t2_em+1e-7;
    //return t2_em;
}

//Check whether box size obeys Fraunhofer diffraction condition for maximum frequency considered and closest antenna
double check_size(std::array<double, 4> cell_id, 
        std::vector<double> antx, std::vector<double> anty, std::vector<double> antz,
        double box_size, double wavelen, double scale) 
{
    //Get minimum antenna distance from cell centerpoint
    double R = DBL_MAX;
    for(std::vector<double>::size_type i = 0; i < antx.size(); i++) {
        double tempR = sqrt( pow(cell_id[0] - antx[i], 2) + pow(cell_id[1]- anty[i], 2) + pow(cell_id[2] - antz[i],2));
        R = std::min(R, tempR);
    }
    
    //Calculate scaled Fraunhofer limit from cell centerpoint to closest antenna (WHY 6PI)
    double fr_box_size = scale*fabs(max_rel_ang * sqrt(R * wavelen/(6*M_PI)));
    
    
    //If box is too big return -1
    return (box_size > fr_box_size) ? -1. : box_size;
}

////////////////////////////////////////////////////////////////////////////

extern "C" {

    void update_vector(int PID, double X1, double Y1, double Z1, double T1, double X2, double Y2, double Z2, double T2, double E1, double E2, double w) {
        
        double L = sqrt(pow(X2 - X1, 2) + pow(Y2 - Y1, 2) + pow(Z2 - Z1, 2));
        
        double Eave = (E1 + E2)/2.;
        double beta = sqrt(1. - 1./pow(Eave/m_e, 2));
        
        double betax = beta * (X2 - X1)/L;
        double betay = beta * (Y2 - Y1)/L;
        double betaz = beta * (Z2 - Z1)/L;

        //Initial Box Size
        double bs = 25.; //meters
        double ts = bs/c * 1e9; //ns
        //std::cout << "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" << std::endl;
        //std::cout << "Track Started" << std::endl;
        //Splitting the track into boxes
        while(T1 < T2) {
            std::array<double, 4> cell_id = {std::floor(X1/bs)*bs + bs/2., std::floor(Y1/bs)*bs + bs/2., std::floor(Z1/bs)*bs + bs/2., std::floor(T1/ts)*ts + ts/2.};
            
            //umap.insert({p1, 10});
            //std::tuple<double,double,double,double> cell_id = {std::floor(X1/bs)*bs + bs/2., std::floor(Y1/bs)*bs + bs/2., std::floor(Z1/bs)*bs + bs/2., std::floor(T1/ts)*ts + ts/2.};
            //std::vector<double> cell_id = {std::floor(X1/bs)*bs + bs/2., std::floor(Y1/bs)*bs + bs/2., std::floor(Z1/bs)*bs + bs/2., std::floor(T1/ts)*ts + ts/2.};

            //If the box is too large, divide into 8 subcells
            while(check_size(cell_id, antx, anty, antz, bs, wavelen, scale) < 0) {
                bs /= 2.; 
                ts /= 2.;
                double xcp = cell_id[0]+bs/2.;
                double ycp = cell_id[1]+bs/2.;
                double zcp = cell_id[2]+bs/2.;
                //double tcp = cell_id[3]+ts/2.;
                
                double xcm = cell_id[0]-bs/2.;
                double ycm = cell_id[1]-bs/2.;
                double zcm = cell_id[2]-bs/2.;
                //double tcm = cell_id[3]-ts/2.;
                
                cell_id[0] = (fabs(X1-xcp) < fabs(X1-xcm)) ? xcp : xcm;
                cell_id[1] = (fabs(Y1-ycp) < fabs(Y1-ycm)) ? ycp : ycm;
                cell_id[2] = (fabs(Z1-zcp) < fabs(Z1-zcm)) ? zcp : zcm;
                //cell_id[3] = (fabs(T1-tcp) < fabs(T1-tcm)) ? tcp : tcm;
                cell_id[3] = std::floor(T1/ts)*ts + ts/2.;
            }
            
            //Find the nearest time edge and the difference between the edge and the track start time
            //std::cout << "Finding T2... " << std::endl;
            //std::cout << "Half cell lengths: " << bs/2. << " " << ts/2. << std::endl;
            //std::cout << "Differences X: " << cell_id[0]-X1 << " Y: "<< cell_id[1]-Y1 << " Z: "<< cell_id[2]-Z1 << " T: "<< cell_id[3]-T1 << std::endl;
            
            double t2_em = find_t2(X1, Y1, Z1, T1, betax, betay, betaz, cell_id, T2, bs, ts);
            double dt_ij = t2_em - T1;
            
            //std::cout.precision(17);
            //std::cout << "Track coordinates X: " << X1 << " Y: "<< Y1 << " Z: "<< Z1 << std::endl;
            //std::cout << "Cell coordinates X: " << cell_id[0] << " Y: "<< cell_id[1] << " Z: "<< cell_id[2] << std::endl;
            //std::cout << "Half cell lengths: " << bs/2. << " " << ts/2. << std::endl;
            //std::cout << "Differences X: " << cell_id[0]-X1 << " Y: "<< cell_id[1]-Y1 << " Z: "<< cell_id[2]-Z1 << " T: "<< cell_id[3]-T1 << std::endl;
            //std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
            
            X2 = X1 + betax * c * dt_ij*1e-9;
            Y2 = Y1 + betay * c * dt_ij*1e-9;
            Z2 = Z1 + betaz * c * dt_ij*1e-9;
            
            //std::this_thread::sleep_for(std::chrono::seconds(5));
            //Go over coordinates with the fellas
           double dist = point_distance(70*M_PI/180., 0*M_PI/180., {X1, Y1, 50000-Z1});
            if(dist<40){
            std::array<double, 2> data = {bs, 0};
            auto [it, grid_insert] = grid.insert({cell_id, data});
            //grid.insert({cell_id, std::vector<double> {bs, 0}});
            //grid[cell_id][1] += 1;
 
	    //grid[cell_id][1] += 1;
	    
//	    if(grid_insert){
//                q += 1;
//	    }
//	    std::cout << q <<std::endl;
            it->second[1] += 1;
	    
            
            }
            
            T1 = t2_em;
            X1 = X2;
            Y1 = Y2;
            Z1 = Z2;
            
        }
        //std::cout << "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" << std::endl;
        //using namespace std::this_thread; // sleep_for, sleep_until
        //using namespace std::chrono; // nanoseconds, system_clock, seconds

        //sleep_for(seconds(1));
    
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
        //std::cout << "Duration: " << duration.count() << std::endl;
        //std::cout<< "Number of total tracks: " << num_parts << std::endl;
        //std::cout<< "Number of split tracks (ZHAireS): " << num_tracks << std::endl;
        //std::cout<< "Number of split tracks after lateral cut (ZHAireS): " << num_tracks_after_cut << std::endl;
        
        
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
    
    }
}
