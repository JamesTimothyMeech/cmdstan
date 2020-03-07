
// Code generated by stanc 159a90f
#include <stan/model/model_header.hpp>
namespace experiments_model_namespace {

template <typename T, typename S>
std::vector<T> resize_to_match__(std::vector<T>& dst, const std::vector<S>& src) {
  dst.resize(src.size());
  return dst;
}

template <typename T>
Eigen::Matrix<T, -1, -1>
resize_to_match__(Eigen::Matrix<T, -1, -1>& dst, const Eigen::Matrix<T, -1, -1>& src) {
  dst.resize(src.rows(), src.cols());
  return dst;
}

template <typename T>
Eigen::Matrix<T, 1, -1>
resize_to_match__(Eigen::Matrix<T, 1, -1>& dst, const Eigen::Matrix<T, 1, -1>& src) {
  dst.resize(src.size());
  return dst;
}

template <typename T>
Eigen::Matrix<T, -1, 1>
resize_to_match__(Eigen::Matrix<T, -1, 1>& dst, const Eigen::Matrix<T, -1, 1>& src) {
  dst.resize(src.size());
  return dst;
}
std::vector<double> to_doubles__(std::initializer_list<double> x) {
  return x;
}

std::vector<stan::math::var> to_vars__(std::initializer_list<stan::math::var> x) {
  return x;
}

inline void validate_positive_index(const char* var_name, const char* expr,
                                    int val) {
  if (val < 1) {
    std::stringstream msg;
    msg << "Found dimension size less than one in simplex declaration"
        << "; variable=" << var_name << "; dimension size expression=" << expr
        << "; expression value=" << val;
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}

inline void validate_unit_vector_index(const char* var_name, const char* expr,
                                       int val) {
  if (val <= 1) {
    std::stringstream msg;
    if (val == 1) {
      msg << "Found dimension size one in unit vector declaration."
          << " One-dimensional unit vector is discrete"
          << " but the target distribution must be continuous."
          << " variable=" << var_name << "; dimension size expression=" << expr;
    } else {
      msg << "Found dimension size less than one in unit vector declaration"
          << "; variable=" << var_name << "; dimension size expression=" << expr
          << "; expression value=" << val;
    }
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}


using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::model_base_crtp;
using stan::model::rvalue;
using stan::model::cons_list;
using stan::model::index_uni;
using stan::model::index_max;
using stan::model::index_min;
using stan::model::index_min_max;
using stan::model::index_multi;
using stan::model::index_omni;
using stan::model::nil_index_list;
using namespace stan::math; 

static int current_statement__ = 0;
static const std::vector<string> locations_array__ = {" (found before start of program)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 8, column 2 to column 13)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 9, column 2 to column 11)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 10, column 2 to column 24)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 11, column 2 to column 24)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 24, column 2 to column 14)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 25, column 2 to column 13)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 26, column 2 to column 13)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 27, column 2 to column 19)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 29, column 4 to column 22)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 30, column 4 to column 28)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 35, column 6 to column 19)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 36, column 6 to column 94)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 34, column 9 to line 37, column 5)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 32, column 6 to column 94)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 33, column 6 to column 19)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 31, column 17 to line 34, column 5)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 31, column 4 to line 37, column 5)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 38, column 4 to column 32)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 28, column 15 to line 39, column 3)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 28, column 2 to line 39, column 3)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 40, column 2 to column 26)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 15, column 3 to column 24)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 16, column 3 to column 22)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 17, column 3 to column 26)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 18, column 3 to column 26)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 21, column 3 to column 58)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 2, column 2 to column 17)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 3, column 2 to column 14)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 4, column 2 to column 14)",
                                                      " (in 'examples/Model-based-Inference-for-Causal-Effects-in-Completely-Randomized-Experiments/experiments.stan', line 5, column 2 to column 29)"};


class experiments_model : public model_base_crtp<experiments_model> {

 private:
  int pos__;
  int N;
  Eigen::Matrix<double, -1, 1> y;
  Eigen::Matrix<double, -1, 1> w;
  double rho;
 
 public:
  ~experiments_model() { }
  
  std::string model_name() const { return "experiments_model"; }
  
  experiments_model(stan::io::var_context& context__,
                    unsigned int random_seed__ = 0,
                    std::ostream* pstream__ = nullptr) : model_base_crtp(0) {
    typedef double local_scalar_t__;
    boost::ecuyer1988 base_rng__ = 
        stan::services::util::create_rng(random_seed__, 0);
    (void) base_rng__;  // suppress unused var warning
    static const char* function__ = "experiments_model_namespace::experiments_model";
    (void) function__;  // suppress unused var warning
    
    try {
      
      pos__ = 1;
      context__.validate_dims("data initialization","N","int",
          context__.to_vec());
      
      current_statement__ = 27;
      pos__ = 1;
      current_statement__ = 27;
      N = context__.vals_i("N")[(pos__ - 1)];
      current_statement__ = 28;
      validate_non_negative_index("y", "N", N);
      context__.validate_dims("data initialization","y","double",
          context__.to_vec(N));
      y = Eigen::Matrix<double, -1, 1>(N);
      
      current_statement__ = 28;
      pos__ = 1;
      current_statement__ = 28;
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        current_statement__ = 28;
        assign(y, cons_list(index_uni(sym1__), nil_index_list()),
          context__.vals_r("y")[(pos__ - 1)], "assigning variable y");
        current_statement__ = 28;
        pos__ = (pos__ + 1);}
      current_statement__ = 29;
      validate_non_negative_index("w", "N", N);
      context__.validate_dims("data initialization","w","double",
          context__.to_vec(N));
      w = Eigen::Matrix<double, -1, 1>(N);
      
      current_statement__ = 29;
      pos__ = 1;
      current_statement__ = 29;
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        current_statement__ = 29;
        assign(w, cons_list(index_uni(sym1__), nil_index_list()),
          context__.vals_r("w")[(pos__ - 1)], "assigning variable w");
        current_statement__ = 29;
        pos__ = (pos__ + 1);}
      context__.validate_dims("data initialization","rho","double",
          context__.to_vec());
      
      current_statement__ = 30;
      pos__ = 1;
      current_statement__ = 30;
      rho = context__.vals_r("rho")[(pos__ - 1)];
      current_statement__ = 27;
      current_statement__ = 27;
      check_greater_or_equal(function__, "N", N, 0);
      current_statement__ = 30;
      current_statement__ = 30;
      check_greater_or_equal(function__, "rho", rho, -1);
      current_statement__ = 30;
      current_statement__ = 30;
      check_less_or_equal(function__, "rho", rho, 1);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    num_params_r__ = 0U;
    
    try {
      num_params_r__ += 1;
      num_params_r__ += 1;
      num_params_r__ += 1;
      num_params_r__ += 1;
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
  }
  template <bool propto__, bool jacobian__, typename T__>
  T__ log_prob(std::vector<T__>& params_r__, std::vector<int>& params_i__,
               std::ostream* pstream__ = 0) const {
    typedef T__ local_scalar_t__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    static const char* function__ = "experiments_model_namespace::log_prob";
(void) function__;  // suppress unused var warning

    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    
    try {
      local_scalar_t__ alpha;
      
      current_statement__ = 1;
      alpha = in__.scalar();
      local_scalar_t__ tau;
      
      current_statement__ = 2;
      tau = in__.scalar();
      local_scalar_t__ sigma_c;
      
      current_statement__ = 3;
      sigma_c = in__.scalar();
      current_statement__ = 3;
      if (jacobian__) {
        current_statement__ = 3;
        sigma_c = stan::math::lb_constrain(sigma_c, 0, lp__);
      } else {
        current_statement__ = 3;
        sigma_c = stan::math::lb_constrain(sigma_c, 0);
      }
      local_scalar_t__ sigma_t;
      
      current_statement__ = 4;
      sigma_t = in__.scalar();
      current_statement__ = 4;
      if (jacobian__) {
        current_statement__ = 4;
        sigma_t = stan::math::lb_constrain(sigma_t, 0, lp__);
      } else {
        current_statement__ = 4;
        sigma_t = stan::math::lb_constrain(sigma_t, 0);
      }
      {
        current_statement__ = 22;
        lp_accum__.add(normal_log<propto__>(alpha, 0, 5));
        current_statement__ = 23;
        lp_accum__.add(normal_log<propto__>(tau, 0, 5));
        current_statement__ = 24;
        lp_accum__.add(normal_log<propto__>(sigma_c, 0, 5));
        current_statement__ = 25;
        lp_accum__.add(normal_log<propto__>(sigma_t, 0, 5));
        current_statement__ = 26;
        lp_accum__.add(
          normal_log<propto__>(y, add(alpha, multiply(tau, w)),
            add(multiply(sigma_t, w), multiply(sigma_c, subtract(1, w)))));
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
    } // log_prob() 
    
  template <typename RNG>
  void write_array(RNG& base_rng__, std::vector<double>& params_r__,
                   std::vector<int>& params_i__, std::vector<double>& vars__,
                   bool emit_transformed_parameters__ = true,
                   bool emit_generated_quantities__ = true,
                   std::ostream* pstream__ = 0) const {
    typedef double local_scalar_t__;
    vars__.resize(0);
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    static const char* function__ = "experiments_model_namespace::write_array";
(void) function__;  // suppress unused var warning

    (void) function__;  // suppress unused var warning

    double lp__ = 0.0;
    (void) lp__;  // dummy to suppress unused var warning
    stan::math::accumulator<double> lp_accum__;
    
    try {
      double alpha;
      
      current_statement__ = 1;
      alpha = in__.scalar();
      double tau;
      
      current_statement__ = 2;
      tau = in__.scalar();
      double sigma_c;
      
      current_statement__ = 3;
      sigma_c = in__.scalar();
      current_statement__ = 3;
      sigma_c = stan::math::lb_constrain(sigma_c, 0);
      double sigma_t;
      
      current_statement__ = 4;
      sigma_t = in__.scalar();
      current_statement__ = 4;
      sigma_t = stan::math::lb_constrain(sigma_t, 0);
      vars__.push_back(alpha);
      vars__.push_back(tau);
      vars__.push_back(sigma_c);
      vars__.push_back(sigma_t);
      if (logical_negation((primitive_value(emit_transformed_parameters__) ||
            primitive_value(emit_generated_quantities__)))) {
        return ;
      } 
      if (logical_negation(emit_generated_quantities__)) {
        return ;
      } 
      double tau_fs;
      
      current_statement__ = 5;
      tau_fs = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 6;
      validate_non_negative_index("y0", "N", N);
      std::vector<double> y0;
      y0 = std::vector<double>(N, 0);
      
      current_statement__ = 7;
      validate_non_negative_index("y1", "N", N);
      std::vector<double> y1;
      y1 = std::vector<double>(N, 0);
      
      current_statement__ = 8;
      validate_non_negative_index("tau_unit", "N", N);
      std::vector<double> tau_unit;
      tau_unit = std::vector<double>(N, 0);
      
      current_statement__ = 20;
      for (size_t n = 1; n <= N; ++n) {
        double mu_c;
        
        current_statement__ = 9;
        mu_c = std::numeric_limits<double>::quiet_NaN();
        current_statement__ = 9;
        mu_c = alpha;
        double mu_t;
        
        current_statement__ = 10;
        mu_t = std::numeric_limits<double>::quiet_NaN();
        current_statement__ = 10;
        mu_t = (alpha + tau);
        current_statement__ = 17;
        if (logical_eq(w[(n - 1)], 1)) {
          current_statement__ = 14;
          assign(y0, cons_list(index_uni(n), nil_index_list()),
            normal_rng(
              (mu_c + ((rho * (sigma_c / sigma_t)) * (y[(n - 1)] - mu_t))),
              (sigma_c * stan::math::sqrt((1 - pow(rho, 2)))), base_rng__),
            "assigning variable y0");
          current_statement__ = 15;
          assign(y1, cons_list(index_uni(n), nil_index_list()), y[(n - 1)],
            "assigning variable y1");
        } else {
          current_statement__ = 11;
          assign(y0, cons_list(index_uni(n), nil_index_list()), y[(n - 1)],
            "assigning variable y0");
          current_statement__ = 12;
          assign(y1, cons_list(index_uni(n), nil_index_list()),
            normal_rng(
              (mu_t + ((rho * (sigma_t / sigma_c)) * (y[(n - 1)] - mu_c))),
              (sigma_t * stan::math::sqrt((1 - pow(rho, 2)))), base_rng__),
            "assigning variable y1");
        }
        current_statement__ = 18;
        assign(tau_unit, cons_list(index_uni(n), nil_index_list()),
          (y1[(n - 1)] - y0[(n - 1)]), "assigning variable tau_unit");}
      current_statement__ = 21;
      tau_fs = mean(tau_unit);
      vars__.push_back(tau_fs);
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        vars__.push_back(y0[(sym1__ - 1)]);}
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        vars__.push_back(y1[(sym1__ - 1)]);}
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        vars__.push_back(tau_unit[(sym1__ - 1)]);}
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // write_array() 
    
  void transform_inits(const stan::io::var_context& context__,
                       std::vector<int>& params_i__,
                       std::vector<double>& vars__, std::ostream* pstream__) const {
    typedef double local_scalar_t__;
    vars__.resize(0);
    vars__.reserve(num_params_r__);
    
    try {
      int pos__;
      
      pos__ = 1;
      double alpha;
      
      current_statement__ = 1;
      pos__ = 1;
      current_statement__ = 1;
      alpha = context__.vals_r("alpha")[(pos__ - 1)];
      double tau;
      
      current_statement__ = 2;
      pos__ = 1;
      current_statement__ = 2;
      tau = context__.vals_r("tau")[(pos__ - 1)];
      double sigma_c;
      
      current_statement__ = 3;
      pos__ = 1;
      current_statement__ = 3;
      sigma_c = context__.vals_r("sigma_c")[(pos__ - 1)];
      current_statement__ = 3;
      sigma_c = stan::math::lb_free(sigma_c, 0);
      double sigma_t;
      
      current_statement__ = 4;
      pos__ = 1;
      current_statement__ = 4;
      sigma_t = context__.vals_r("sigma_t")[(pos__ - 1)];
      current_statement__ = 4;
      sigma_t = stan::math::lb_free(sigma_t, 0);
      vars__.push_back(alpha);
      vars__.push_back(tau);
      vars__.push_back(sigma_c);
      vars__.push_back(sigma_t);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // transform_inits() 
    
  void get_param_names(std::vector<std::string>& names__) const {
    
    names__.resize(0);
    names__.push_back("alpha");
    names__.push_back("tau");
    names__.push_back("sigma_c");
    names__.push_back("sigma_t");
    names__.push_back("tau_fs");
    names__.push_back("y0");
    names__.push_back("y1");
    names__.push_back("tau_unit");
    } // get_param_names() 
    
  void get_dims(std::vector<std::vector<size_t>>& dimss__) const {
    dimss__.resize(0);
    std::vector<size_t> dims__;
    dimss__.push_back(dims__);
    dims__.resize(0);
    dimss__.push_back(dims__);
    dims__.resize(0);
    dimss__.push_back(dims__);
    dims__.resize(0);
    dimss__.push_back(dims__);
    dims__.resize(0);
    dimss__.push_back(dims__);
    dims__.resize(0);
    dims__.push_back(N);
    dimss__.push_back(dims__);
    dims__.resize(0);
    dims__.push_back(N);
    dimss__.push_back(dims__);
    dims__.resize(0);
    dims__.push_back(N);
    dimss__.push_back(dims__);
    dims__.resize(0);
    
    } // get_dims() 
    
  void constrained_param_names(std::vector<std::string>& param_names__,
                               bool emit_transformed_parameters__ = true,
                               bool emit_generated_quantities__ = true) const {
    
    param_names__.push_back(std::string() + "alpha");
    param_names__.push_back(std::string() + "tau");
    param_names__.push_back(std::string() + "sigma_c");
    param_names__.push_back(std::string() + "sigma_t");
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      param_names__.push_back(std::string() + "tau_fs");
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        {
          param_names__.push_back(std::string() + "y0" + '.' + std::to_string(sym1__));
        }}
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        {
          param_names__.push_back(std::string() + "y1" + '.' + std::to_string(sym1__));
        }}
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        {
          param_names__.push_back(std::string() + "tau_unit" + '.' + std::to_string(sym1__));
        }}
    }
    
    } // constrained_param_names() 
    
  void unconstrained_param_names(std::vector<std::string>& param_names__,
                                 bool emit_transformed_parameters__ = true,
                                 bool emit_generated_quantities__ = true) const {
    
    param_names__.push_back(std::string() + "alpha");
    param_names__.push_back(std::string() + "tau");
    param_names__.push_back(std::string() + "sigma_c");
    param_names__.push_back(std::string() + "sigma_t");
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      param_names__.push_back(std::string() + "tau_fs");
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        {
          param_names__.push_back(std::string() + "y0" + '.' + std::to_string(sym1__));
        }}
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        {
          param_names__.push_back(std::string() + "y1" + '.' + std::to_string(sym1__));
        }}
      for (size_t sym1__ = 1; sym1__ <= N; ++sym1__) {
        {
          param_names__.push_back(std::string() + "tau_unit" + '.' + std::to_string(sym1__));
        }}
    }
    
    } // unconstrained_param_names() 
    
  std::string get_constrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"alpha\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"tau\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma_c\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma_t\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"tau_fs\",\"type\":{\"name\":\"real\"},\"block\":\"generated_quantities\"},{\"name\":\"y0\",\"type\":{\"name\":\"array\",\"length\":" << N << ",\"element_type\":{\"name\":\"real\"}},\"block\":\"generated_quantities\"},{\"name\":\"y1\",\"type\":{\"name\":\"array\",\"length\":" << N << ",\"element_type\":{\"name\":\"real\"}},\"block\":\"generated_quantities\"},{\"name\":\"tau_unit\",\"type\":{\"name\":\"array\",\"length\":" << N << ",\"element_type\":{\"name\":\"real\"}},\"block\":\"generated_quantities\"}]";
    return s__.str();
    } // get_constrained_sizedtypes() 
    
  std::string get_unconstrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"alpha\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"tau\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma_c\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma_t\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"tau_fs\",\"type\":{\"name\":\"real\"},\"block\":\"generated_quantities\"},{\"name\":\"y0\",\"type\":{\"name\":\"array\",\"length\":" << N << ",\"element_type\":{\"name\":\"real\"}},\"block\":\"generated_quantities\"},{\"name\":\"y1\",\"type\":{\"name\":\"array\",\"length\":" << N << ",\"element_type\":{\"name\":\"real\"}},\"block\":\"generated_quantities\"},{\"name\":\"tau_unit\",\"type\":{\"name\":\"array\",\"length\":" << N << ",\"element_type\":{\"name\":\"real\"}},\"block\":\"generated_quantities\"}]";
    return s__.str();
    } // get_unconstrained_sizedtypes() 
    
  
    // Begin method overload boilerplate
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool emit_transformed_parameters__ = true,
                     bool emit_generated_quantities__ = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng__, params_r_vec, params_i_vec, vars_vec,
          emit_transformed_parameters__, emit_generated_quantities__, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    template <bool propto__, bool jacobian__, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto__,jacobian__,T_>(vec_params_r, vec_params_i, pstream);
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }

};
}
typedef experiments_model_namespace::experiments_model stan_model;

#ifndef USING_R

// Boilerplate
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}

#endif


