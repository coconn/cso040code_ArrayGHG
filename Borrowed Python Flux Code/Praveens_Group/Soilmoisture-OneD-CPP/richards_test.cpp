#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <cmath>
#include <iostream>
#include <cstdlib>
#include <complex>
#include <cstring>
#include <algorithm>

#include "Eigen/Dense"
#include "netcdf.h"

#define alpha 0.02            // [1/cm]
#define nv 1.41               // [-]
#define theta_R 0.067         // [-]
#define theta_S 0.45          // [-]
#define Ss 5e-4               // [1/m]
#define poros 0.45            // [-]
#define stop_tol 0.00005      // [m]
#define stop_tol_mois 0.0001  // [-]
#define Psimin  0.0005        // [m]
#define air_dry -10.0         // [m]
#define maxiter 100           // [-]
#define print 0
#define am 0.25               // Weight for modified Picard

using namespace Eigen;
using namespace std;

template <class T> const T& maxcomp (const T& a, const T& b) {
  return (a < b) ? b : a;
}

template <class T> const T& mincomp (const T& a, const T& b) {
  return (a > b) ? b : a;
}

// --------------------------------------------------------------------
// LoadVariable()
//    Loads variables from NetCDF file into memory.
//    This function uses NetCDF3 lib and may need to adjust for NetCDF4
// --------------------------------------------------------------------
template<typename Type> void LoadVariable (
    const char *file_name,  // File name loaded
    const char *var_name,   // Name of variable in NetCDF will be loaded
    Type *data)             // Data assigned to memory
{
  int ncid,                 // NetCDF file ID
      varid;                // Variable ID
  
  // Open Netcdf file with NC_NOWRITE options (for Loading)
  nc_open(file_name, NC_NOWRITE, &ncid);

  nc_inq_varid(ncid, var_name, &varid);     // Get variable ID  
  nc_get_var(ncid, varid, &data[0]);        // Get data based on data_type
  nc_close(ncid);                           // Close the NetCDF file
}



// --------------------------------------------------------------------
// SaveVariables1D()
//    Saves one-dimensional variables into a NetCDF file.
//    This function uses NetCDF3 lib and may need to adjust for NetCDF4
// --------------------------------------------------------------------
template<typename Type> void save_output1D (
    const char *file,       // File name saved
    int length_data,        // Size of variable saved
    const char *dim_name,   // Name of dimension of variable
    const char *data_name,  // Name of variable saved to NetCDF
    Type *data_in,          // Data saved to NetCDF file
    nc_type NC_DATATYPE,    // Type of data
    int write )             // Value identifies create new or open existing file
{
  int ncid,                 // NetCDF file ID
      varid,                // Variable ID
      t_dimid,              // time dimension
      dimids[1];            // Dimension ID (1 for 1D)

  // Set up NetCDF file for writing
  if (write == 0){  // Create a new NetCDF file
    nc_create(file, NC_CLOBBER, &ncid); 
  } else {          // Open and re-define an existing NetCDF file
    nc_open(file, NC_WRITE, &ncid);
    nc_redef(ncid);
  }

  // Define the dimension 1D of variable
  nc_def_dim(ncid, dim_name, length_data, &t_dimid);
  dimids[0] = t_dimid;

  // Define variable's NetCDF format  
  nc_def_var(ncid, data_name, NC_DATATYPE, 1, dimids, &varid);
  nc_enddef(ncid);

  nc_put_var(ncid, varid, &data_in[0]); // Write data to NetCDF file
  nc_close(ncid);                       // Close the NetCDF file
}


// --------------------------------------------------------------------
// SaveVariables1D()
//    Saves 1D double type variables into a NetCDF file.
// --------------------------------------------------------------------
void SaveVariables1D(const char *file, int time_steps, const char *var_name, 
    double *var, int write) 
{
  int status, ncid, t_dimid, varid, dimids[1];

  if (write == 0){
    nc_create(file, NC_CLOBBER, &ncid); 
  } else {
    nc_open(file, NC_WRITE, &ncid);
    nc_redef(ncid);
  }

  nc_def_dim(ncid, "x", time_steps, &t_dimid);
  dimids[0] = t_dimid;

  nc_def_var(ncid, var_name, NC_DOUBLE, 1, dimids, &varid);

  nc_enddef(ncid);

  nc_put_var_double(ncid, varid, &var[0]);
  nc_close(ncid);
}


// --------------------------------------------------------------------
// SaveOneOutput2D()
//    Saves one/single double type 2D variable into a NetCDF file.
// --------------------------------------------------------------------
void SaveOneOutput2D(const char *file, int Nx, int My, const char *data_name,
    double *data_in, int write) 
{
  int ncid, x_dimid, y_dimid, varid, dimids[2];

  if (write == 0){
    nc_create(file, NC_CLOBBER, &ncid);
  } else {
    nc_open(file, NC_WRITE, &ncid);
    nc_redef(ncid);
  }
    
  nc_def_dim(ncid, "x", Nx, &x_dimid);
  nc_def_dim(ncid, "y", My, &y_dimid);
  dimids[0] = y_dimid;
  dimids[1] = x_dimid;
  nc_def_var(ncid, data_name, NC_DOUBLE, 2, dimids, &varid);
  nc_enddef(ncid);
  nc_put_var_double(ncid, varid, &data_in[0]);
  nc_close(ncid);
}

double maxError(VectorXd& myArray, int SIZE) 
{
  int i;
  double maxError = std::abs(myArray[0]);

  for (i = 1; i < SIZE; ++i) {
    if ( std::abs(myArray[i]) > maxError ){
      maxError = std::abs(myArray[i]);
    }
  }

  return maxError;
}

void ThomasAlgorithm(VectorXd& a, VectorXd& b, VectorXd& c, VectorXd& d, 
    VectorXd& f, int N) 
{
  double m;
  std::vector<double> c_star(N, 0.0);
  std::vector<double> d_star(N, 0.0);

  // Modify the first-row coefficients
  c_star[0] = c[0] / b[0];
  d_star[0] = d[0] / b[0];

  // Forward sweep and modification
  for (int i = 1; i < N; i++) {
    m = 1.0 / (b[i] - a[i] * c_star[i-1]);
    c_star[i] = c[i] * m;
    d_star[i] = (d[i] - a[i] * d_star[i-1]) * m;
  }

  // Backward substitution
  f[N-1] = d_star[N-1];
  for (int i = N-2; i >= 0; i--) {
    f[i] = d_star[i] - c_star[i] * f[i+1];
  }
}


void vanGenuchten(VectorXd& C, VectorXd& K, VectorXd& Ksat, VectorXd& theta, 
    VectorXd& h, double n, int SIZE)
{
  double m = 1.0 - 1.0/n;
  double Se;
  double h_, theta_;

  for (int i=0; i<SIZE; i++){
    h_ = h[i] * 100;            // [cm] Convert to centimeter

    // . . .Compute the volumetric moisture content [eqn 21] . . .
    if (h_ < 0){
      theta[i] = (theta_S - theta_R) / pow(1.0 + pow((alpha*(-h_)),n), m) + theta_R;
    }
    else{
      theta[i] = theta_S;
    }

    // . . .Compute the effective saturation [eqn 2] . . .
    Se = (theta[i] - theta_R)/(theta_S - theta_R);  // [-]

    // . . .Compute the hydraulic conductivity [eqn 8] . . .
    K[i] = Ksat[i] * sqrt(Se) * (1.0 - pow(1.0-pow(Se,1.0/m),m))*(1.0 - pow(1.0-pow(Se,1.0/m),m));     // [ unit: mm/s ]

    // . . .Compute the specific moisture storage (derivative of eqn 21: C = d(theta)/dh . . .
    if (h_ < 0){
      C[i] = -alpha * n * -1 * (1.0/n-1.0)*pow(alpha*std::abs(h_),n-1) * (theta_R-theta_S) * pow(pow(alpha*std::abs(h_),n)+1,1.0/n-2.0) * 100; // [ Unit: 1/mm ]
    }
    else{
      C[i] = 0.0;
    }
  }
}

void vanGenuchten_inverse(VectorXd& theta, VectorXd& h, double n, int SIZE)
{
  double m = 1.0 - 1.0/n;
  for (int i=0; i<SIZE; i++){    
    if (theta[i] < theta_S){
      h[i] = -(1/alpha) * pow(pow((theta_S-theta_R)/(theta[i]-theta_R),1/m)-1.0,1/n) * 0.01;  // [m]
    } else {
      h[i] = 0;
    }
  }
}

void Set_Matrix(VectorXd& a, VectorXd& b, VectorXd& c, VectorXd& d, 
    VectorXd& Cnp1m, VectorXd& Knp1m, VectorXd& hnp1m, VectorXd& hn, 
    VectorXd& thetanp1m, VectorXd& thetan, VectorXd& smp, VectorXd& dz, 
    int P, int TopBound, double htop, double qin, int BotBound, double hbottom, 
    double qout, double dt)
{
  double Knp1m_up;
  double Knp1m_down;

  for (int k=0; k<P; k++){
    if (k==0 || k==P-1){
      Knp1m_up = Knp1m[k];
      Knp1m_down = Knp1m[k];
    } else {
      Knp1m_up = (Knp1m[k+1]-Knp1m[k])/(dz[k]+dz[k+1])*dz[k] + Knp1m[k];
      Knp1m_down = (Knp1m[k]-Knp1m[k-1])/(dz[k]+dz[k-1])*dz[k-1] + Knp1m[k-1];
    }

    if (k==0 || k==P-1){
      b[k] = (Cnp1m[k]+Ss*thetanp1m[k]/poros)/(dt);
      a[k] = 0.0;
      c[k] = 0.0;
    } else {
      b[k] = (Cnp1m[k]+Ss*thetanp1m[k]/poros)/(dt) 
              + (1/dz[k])*(2*Knp1m_up/(dz[k+1]+dz[k]) + 2*Knp1m_down/(dz[k]+dz[k-1]));
      a[k] = -2.0*Knp1m_down/(dz[k]+dz[k-1])/dz[k];
      c[k] = -2.0*Knp1m_up/(dz[k+1]+dz[k])/dz[k];
    }
    d[k] = Cnp1m[k]/(dt)*hnp1m[k] + (Ss*thetanp1m[k]/poros)*hn[k]/(dt)
         - (1.0/dz[k])*(Knp1m_up - Knp1m_down) - (thetanp1m[k] - thetan[k])/(dt);

    if (TopBound == 0) {
      if (k==0){
        b[k] = 1;
        d[k] = htop;
      }
      if (k==1){
        a[k] = 0;            
        d[k] += 2.0*Knp1m_down/(dz[k]+dz[k-1])/dz[k] * htop;
      }
    } else {
      if (k==0){
        b[k] += 2.0*Knp1m_up/(dz[k+1]+dz[k]) / dz[k];
        c[k] = -2.0*Knp1m_up/(dz[k+1]+dz[k]) / dz[k];
        d[k] += ( -Knp1m[k] + qin ) / dz[k];
      }
    }

    if (BotBound == 0) {
      if (k==P-1){
        b[k] = 1;
        d[k] = hbottom;
      }
      if (k==P-2){
        c[k] = 0;                
        d[k] += 2.0*Knp1m_up/(dz[k+1]+dz[k])/dz[k] * hbottom;
      }
    } else {
      if (k==P-1){
        b[k] += 2.0*Knp1m_down/(dz[k]+dz[k-1])/dz[k];
        a[k] = -2.0*Knp1m_down/(dz[k]+dz[k-1])/dz[k];
        d[k] += ( Knp1m[k] + qout ) / dz[k];
      }
    }   
  }
}


void SoilModel(VectorXd& hn, VectorXd& thetan, VectorXd& Ksat, VectorXd& smp, 
    VectorXd& PPT, VectorXd& ET, VectorXd& qss, VectorXd& PH, VectorXd& htb, 
    VectorXd& dz, VectorXd& Bound, VectorXd& case_out, int BotBound, 
    double hbottom, double qout, int TopBound, double htop, double qin, 
    double dt, int t, int P, int *niter_out)
{
  int stop_flag, niter, cases;
  double qcap, hpot, hrem, ph_loc, phong;
  VectorXd hnp1m(P), hnp1mp1(P), thetanp1m(P), thetanp1mp1(P), Knp1m(P), Cnp1m(P);
  VectorXd a_z(P), b_z(P), c_z(P), d_z(P), f_z(P);

  hnp1m = hn;
  stop_flag = 0;
  niter = 0;
  qin = 0;

  if (t == 1006) {
    printf("Checking point \n");
  }
  while (stop_flag == 0 && niter < maxiter) {
    vanGenuchten(Cnp1m, Knp1m, Ksat, thetanp1m, hnp1m, nv, P);

    htop = htb[t-1];
    ph_loc = PH[t-1];
    
    if (htop > Psimin) {
      // Saturated or Ponding conditions,  Dirichlet BC type
      TopBound = 0;
      htop = PH[t-1] + PPT[t] + ET[t];
      cases = 1;
    } else {
      // Non-ponding or below saturation condition
      if (htop > air_dry){
        // 2. Unsaturated or Air-dry condition
        TopBound = 1;
        hpot = PH[t-1] + PPT[t] + ET[t];
        qcap = -Knp1m[0] * (hnp1m[0] - hpot - dz[0]*0.0) / dz[0];
        qin = mincomp(qcap, hpot/dt);
        hrem = maxcomp(hpot - qin*dt, 0.0);
        cases = 2;
      } else {
        // 3. Air-dry condition, Dirichlet BC type
        TopBound = 0;
        htop = air_dry + 2.0;
        cases = 3;
      }
    }

    qout = -Knp1m[P-1];
    Set_Matrix(a_z, b_z, c_z, d_z, Cnp1m, Knp1m, hnp1m, hn, thetanp1m,
        thetan, smp, dz, P, TopBound, htop, qin, BotBound, 
        hbottom, qout, dt);
    ThomasAlgorithm(a_z, b_z, c_z, d_z, hnp1mp1, P);
    vanGenuchten(Cnp1m, Knp1m, Ksat, thetanp1mp1, hnp1mp1, nv, P);

    niter += 1;      

    //f_z = hnp1mp1 - hnp1m;
    f_z = thetanp1mp1 - thetanp1m;
    phong = maxError(f_z, P);
    if (maxError(f_z, P) < stop_tol_mois){
      stop_flag = 1;
    } else {
      hnp1m = am*hnp1m + (1-am)*hnp1mp1;
    }
    
    vanGenuchten(Cnp1m, Knp1m, Ksat, thetanp1m, hnp1mp1, nv, P);        
    if (TopBound == 1){
      qss[t] = qin;
      //htop = hnp1mp1[0] - dz[0] + qss[t] * dz[0] / Knp1m[0];
      PH[t] = maxcomp(hpot - qin*dt, 0.0);
    } else {
      qss[t] = -Knp1m[0] * (hnp1mp1[0] - htop - dz[0]) / dz[0];
      //htop -= qss[t]*dt;
      PH[t] = maxcomp(hnp1mp1[0], 0.0);
    }
    ph_loc = PH[t];
  }
  htb[t] = hnp1mp1[0];
  *niter_out = niter;
  Bound[t] = TopBound;
  case_out[t] = cases;
  thetan = thetanp1m;
  hn = hnp1mp1;
}


//------------------------------------------------------------------------------
// Main Program . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
//------------------------------------------------------------------------------
int main()
{
  std::cout.precision(15);  
  char forcings[32];
  double qout, qin, hbottom, htop;  
  int BotBound, TopBound, PPTend;

  int P =  26;
  int numsteps;
  double dt = 0.5;          // [hr]

  //... Idenfity number of time steps
  snprintf(forcings, 32, "forcings_7536.nc");
  LoadVariable(forcings, "num_steps", &numsteps);

  VectorXd hn(P), thetan(P), Ksat(P), dz(P);
  VectorXd hnp1m(P), hnp1mp1(P), thetanp1m(P), Knp1m(P), Cnp1m(P);
  VectorXd zns(P), zhs(P), rootfr(P), TR_soil(P), smp(P);
  VectorXd PPT(numsteps), ET(numsteps), TR(numsteps), qss(numsteps);
  VectorXd PH(numsteps), htb(numsteps), Bound(numsteps), Cases(numsteps);
  double *PPT_in = new double[numsteps];

  PH = VectorXd::Zero(numsteps);
  htb = VectorXd::Zero(numsteps);
  qss = VectorXd::Zero(numsteps);
  Bound = VectorXd::Zero(numsteps);
  Cases = VectorXd::Zero(numsteps);
  thetan = VectorXd::LinSpaced(P, 0.3, 0.3);

  vanGenuchten_inverse(thetan, hn, nv, P); 
  Ksat << VectorXd::Ones(P)*5.0e-3; // [m/hr]

  // Setup rainfall and ET info  
  LoadVariable(forcings, "PPT_in", PPT_in);  

  double PPTsum = 0;
  for (int i=0; i<10; i++) {
    //PPT_in[i] = 2.0;    
  }

  for (int t=0; t<numsteps; t++){
    PPT[t] = PPT_in[t]/1000;
    PPTsum += PPT[t];    
  }
  std::cout << "PPTsum = " << PPTsum << std::endl;

  dz = VectorXd::Ones(P) * 0.05;

  // Set Bottom BCs and info
  BotBound = 1;
  qout = 0.0;
  hbottom = -4.0;  // [mm]

  TopBound = 1;
  qin = 0.0;
  hbottom = -0.3;  // [mm]

  double *h_store = new double[P*numsteps];
  double *theta_store = new double[P*numsteps];
  double *PH_store = new double[numsteps];
  double *qss_store = new double[numsteps];
  double *bound_store = new double[numsteps];
  double *cases_store = new double[numsteps];  
  int *niter_store = new int[numsteps];
  double *dz_store = new double[P];
  int niter;

  vanGenuchten(Cnp1m, Knp1m, Ksat, thetan, hn, nv, P);
  for (int i=0; i<P; i++){
    h_store[i] = hn[i];
    theta_store[i] = thetan[i];
    dz_store[i] = dz[i];
  }
  PH_store[0] = 0;

  cout << "Numsteps = " << numsteps << endl;
  for (int t=1; t<numsteps; t++){
    // Call SoilModel function
    SoilModel(hn, thetan, Ksat, smp, PPT, ET, qss, PH, htb, dz, Bound, Cases, 
      BotBound, hbottom, qout, TopBound, htop, qin, dt, t, P, &niter);    
    
    // Copy vectors to stored variables
    for (int i=0; i<P; i++){
      h_store[t*P+i] = hn[i];
      theta_store[t*P+i] = thetan[i];
    }
    PH_store[t] = PH[t];
    qss_store[t] = qss[t];
    bound_store[t] = Bound[t];
    cases_store[t] = Cases[t];
    niter_store[t] = niter;
  }

  // Save to NetCDF format
  printf("Saving Outputs. . .\n");
  char file_1D[32], file_2D[32], file_struct[32];
  snprintf(file_1D, sizeof(char) * 32, "OneD_store.nc");
  snprintf(file_2D, sizeof(char) * 32, "TwoD_store.nc");
  snprintf(file_struct, sizeof(char) * 32, "Struct_store.nc");

  save_output1D(file_1D, numsteps, "time", "PH", PH_store, NC_DOUBLE, 0);
  save_output1D(file_1D, numsteps, "time", "niter", niter_store, NC_INT, 1);
  save_output1D(file_1D, numsteps, "time", "qss", qss_store, NC_DOUBLE, 1);
  save_output1D(file_1D, numsteps, "time", "bound", bound_store, NC_DOUBLE, 1);
  save_output1D(file_1D, numsteps, "time", "cases", cases_store, NC_DOUBLE, 1);

  save_output1D(file_struct, P, "vert", "dz", dz_store, NC_DOUBLE, 0);

  SaveOneOutput2D(file_2D, P, numsteps, "psi", h_store, 0);
  SaveOneOutput2D(file_2D, P, numsteps, "theta", theta_store, 1);
}
