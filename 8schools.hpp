// Code generated by %%NAME%% %%VERSION%%
#include <stan/model/model_header.hpp>
namespace _8schools_model_namespace {
using stan::model::model_base_crtp;
using namespace stan::math;
stan::math::profile_map profiles__;
static constexpr std::array<const char*, 14> locations_array__ =
  {" (found before start of program)",
  " (in '8schools.stan', line 8, column 2 to column 10)",
  " (in '8schools.stan', line 9, column 2 to column 20)",
  " (in '8schools.stan', line 10, column 2 to column 18)",
  " (in '8schools.stan', line 17, column 2 to column 20)",
  " (in '8schools.stan', line 18, column 2 to column 21)",
  " (in '8schools.stan', line 19, column 2 to column 26)",
  " (in '8schools.stan', line 20, column 2 to column 27)",
  " (in '8schools.stan', line 2, column 2 to column 17)",
  " (in '8schools.stan', line 3, column 9 to column 10)",
  " (in '8schools.stan', line 3, column 2 to column 14)",
  " (in '8schools.stan', line 4, column 18 to column 19)",
  " (in '8schools.stan', line 4, column 2 to column 32)",
  " (in '8schools.stan', line 10, column 9 to column 10)"};
class _8schools_model final : public model_base_crtp<_8schools_model> {
 private:
  int J;
  Eigen::Matrix<double,-1,1> y_data__;
  Eigen::Matrix<double,-1,1> sigma_data__;
  Eigen::Map<Eigen::Matrix<double,-1,1>> y{nullptr, 0};
  Eigen::Map<Eigen::Matrix<double,-1,1>> sigma{nullptr, 0};
 public:
  ~_8schools_model() {}
  _8schools_model(stan::io::var_context& context__, unsigned int
                  random_seed__ = 0, std::ostream* pstream__ = nullptr)
      : model_base_crtp(0) {
    int current_statement__ = 0;
    // suppress unused var warning
    (void) current_statement__;
    using local_scalar_t__ = double;
    auto base_rng__ = stan::services::util::create_rng(random_seed__, 0);
    // suppress unused var warning
    (void) base_rng__;
    static constexpr const char* function__ =
      "_8schools_model_namespace::_8schools_model";
    // suppress unused var warning
    (void) function__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      current_statement__ = 8;
      context__.validate_dims("data initialization", "J", "int",
        std::vector<size_t>{});
      J = std::numeric_limits<int>::min();
      current_statement__ = 8;
      J = context__.vals_i("J")[(1 - 1)];
      current_statement__ = 8;
      stan::math::check_greater_or_equal(function__, "J", J, 0);
      current_statement__ = 9;
      stan::math::validate_non_negative_index("y", "J", J);
      current_statement__ = 10;
      context__.validate_dims("data initialization", "y", "double",
        std::vector<size_t>{static_cast<size_t>(J)});
      y_data__ = Eigen::Matrix<double,-1,1>::Constant(J,
                   std::numeric_limits<double>::quiet_NaN());
      new (&y) Eigen::Map<Eigen::Matrix<double,-1,1>>(y_data__.data(), J);
      {
        std::vector<local_scalar_t__> y_flat__;
        current_statement__ = 10;
        y_flat__ = context__.vals_r("y");
        pos__ = 1;
        for (int sym1__ = 1; sym1__ <= J; ++sym1__) {
          stan::model::assign(y, y_flat__[(pos__ - 1)],
            "assigning variable y", stan::model::index_uni(sym1__));
          pos__ = (pos__ + 1);
        }
      }
      current_statement__ = 11;
      stan::math::validate_non_negative_index("sigma", "J", J);
      current_statement__ = 12;
      context__.validate_dims("data initialization", "sigma", "double",
        std::vector<size_t>{static_cast<size_t>(J)});
      sigma_data__ = Eigen::Matrix<double,-1,1>::Constant(J,
                       std::numeric_limits<double>::quiet_NaN());
      new (&sigma)
        Eigen::Map<Eigen::Matrix<double,-1,1>>(sigma_data__.data(), J);
      {
        std::vector<local_scalar_t__> sigma_flat__;
        current_statement__ = 12;
        sigma_flat__ = context__.vals_r("sigma");
        pos__ = 1;
        for (int sym1__ = 1; sym1__ <= J; ++sym1__) {
          stan::model::assign(sigma, sigma_flat__[(pos__ - 1)],
            "assigning variable sigma", stan::model::index_uni(sym1__));
          pos__ = (pos__ + 1);
        }
      }
      current_statement__ = 12;
      stan::math::check_greater_or_equal(function__, "sigma", sigma, 0);
      current_statement__ = 13;
      stan::math::validate_non_negative_index("theta", "J", J);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    num_params_r__ = 1 + 1 + J;
  }
  inline std::string model_name() const final {
    return "_8schools_model";
  }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = %%NAME%%3 %%VERSION%%",
             "stancflags = --marginalize"};
  }
  // Base log prob
  template <bool propto__, bool jacobian__, typename VecR, typename VecI,
            stan::require_vector_like_t<VecR>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr,
            stan::require_not_st_var<VecR>* = nullptr>
  inline stan::scalar_type_t<VecR>
  log_prob_impl(VecR& params_r__, VecI& params_i__, std::ostream*
                pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    int current_statement__ = 0;
    // suppress unused var warning
    (void) current_statement__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    static constexpr const char* function__ =
      "_8schools_model_namespace::log_prob";
    // suppress unused var warning
    (void) function__;
    try {
      local_scalar_t__ mu = DUMMY_VAR__;
      current_statement__ = 1;
      mu = in__.template read<local_scalar_t__>();
      local_scalar_t__ tau = DUMMY_VAR__;
      current_statement__ = 2;
      tau = in__.template read_constrain_lb<local_scalar_t__, jacobian__>(0,
              lp__);
      Eigen::Matrix<local_scalar_t__,-1,1> theta =
        Eigen::Matrix<local_scalar_t__,-1,1>::Constant(J, DUMMY_VAR__);
      current_statement__ = 3;
      theta = in__.template read<Eigen::Matrix<local_scalar_t__,-1,1>>(J);
      {
        current_statement__ = 4;
        lp_accum__.add(stan::math::normal_lpdf<propto__>(mu, 0, 5));
        current_statement__ = 5;
        lp_accum__.add(stan::math::cauchy_lpdf<propto__>(tau, 0, 5));
        current_statement__ = 6;
        lp_accum__.add(stan::math::normal_lpdf<propto__>(theta, mu, tau));
        current_statement__ = 7;
        lp_accum__.add(stan::math::normal_lpdf<propto__>(y, theta, sigma));
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
  }
  // Reverse mode autodiff log prob
  template <bool propto__, bool jacobian__, typename VecR, typename VecI,
            stan::require_vector_like_t<VecR>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr,
            stan::require_st_var<VecR>* = nullptr>
  inline stan::scalar_type_t<VecR>
  log_prob_impl(VecR& params_r__, VecI& params_i__, std::ostream*
                pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    int current_statement__ = 0;
    // suppress unused var warning
    (void) current_statement__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    static constexpr const char* function__ =
      "_8schools_model_namespace::log_prob";
    // suppress unused var warning
    (void) function__;
    try {
      local_scalar_t__ mu = DUMMY_VAR__;
      current_statement__ = 1;
      mu = in__.template read<local_scalar_t__>();
      local_scalar_t__ tau = DUMMY_VAR__;
      current_statement__ = 2;
      tau = in__.template read_constrain_lb<local_scalar_t__, jacobian__>(0,
              lp__);
      Eigen::Matrix<local_scalar_t__,-1,1> theta =
        Eigen::Matrix<local_scalar_t__,-1,1>::Constant(J, DUMMY_VAR__);
      current_statement__ = 3;
      theta = in__.template read<Eigen::Matrix<local_scalar_t__,-1,1>>(J);
      {
        current_statement__ = 4;
        lp_accum__.add(stan::math::normal_lpdf<propto__>(mu, 0, 5));
        current_statement__ = 5;
        lp_accum__.add(stan::math::cauchy_lpdf<propto__>(tau, 0, 5));
        current_statement__ = 6;
        lp_accum__.add(stan::math::normal_lpdf<propto__>(theta, mu, tau));
        current_statement__ = 7;
        lp_accum__.add(stan::math::normal_lpdf<propto__>(y, theta, sigma));
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
  }
  template <typename RNG, typename VecR, typename VecI, typename VecVar,
            stan::require_vector_like_vt<std::is_floating_point,
            VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral,
            VecI>* = nullptr, stan::require_vector_vt<std::is_floating_point,
            VecVar>* = nullptr>
  inline void
  write_array_impl(RNG& base_rng__, VecR& params_r__, VecI& params_i__,
                   VecVar& vars__, const bool
                   emit_transformed_parameters__ = true, const bool
                   emit_generated_quantities__ = true, std::ostream*
                   pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    stan::io::serializer<local_scalar_t__> out__(vars__);
    static constexpr bool propto__ = true;
    // suppress unused var warning
    (void) propto__;
    double lp__ = 0.0;
    // suppress unused var warning
    (void) lp__;
    int current_statement__ = 0;
    // suppress unused var warning
    (void) current_statement__;
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    constexpr bool jacobian__ = false;
    // suppress unused var warning
    (void) jacobian__;
    static constexpr const char* function__ =
      "_8schools_model_namespace::write_array";
    // suppress unused var warning
    (void) function__;
    try {
      double mu = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 1;
      mu = in__.template read<local_scalar_t__>();
      double tau = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 2;
      tau = in__.template read_constrain_lb<local_scalar_t__, jacobian__>(0,
              lp__);
      Eigen::Matrix<double,-1,1> theta =
        Eigen::Matrix<double,-1,1>::Constant(J,
          std::numeric_limits<double>::quiet_NaN());
      current_statement__ = 3;
      theta = in__.template read<Eigen::Matrix<local_scalar_t__,-1,1>>(J);
      out__.write(mu);
      out__.write(tau);
      out__.write(theta);
      if (stan::math::logical_negation(
            (stan::math::primitive_value(emit_transformed_parameters__) ||
            stan::math::primitive_value(emit_generated_quantities__)))) {
        return ;
      }
      if (stan::math::logical_negation(emit_generated_quantities__)) {
        return ;
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  template <typename VecVar, typename VecI,
            stan::require_vector_t<VecVar>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline void
  unconstrain_array_impl(const VecVar& params_r__, const VecI& params_i__,
                         VecVar& vars__, std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    stan::io::serializer<local_scalar_t__> out__(vars__);
    int current_statement__ = 0;
    // suppress unused var warning
    (void) current_statement__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      local_scalar_t__ mu = DUMMY_VAR__;
      current_statement__ = 1;
      mu = in__.read<local_scalar_t__>();
      out__.write(mu);
      local_scalar_t__ tau = DUMMY_VAR__;
      current_statement__ = 2;
      tau = in__.read<local_scalar_t__>();
      out__.write_free_lb(0, tau);
      Eigen::Matrix<local_scalar_t__,-1,1> theta =
        Eigen::Matrix<local_scalar_t__,-1,1>::Constant(J, DUMMY_VAR__);
      current_statement__ = 3;
      stan::model::assign(theta,
        in__.read<Eigen::Matrix<local_scalar_t__,-1,1>>(J),
        "assigning variable theta");
      out__.write(theta);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  template <typename VecVar, stan::require_vector_t<VecVar>* = nullptr>
  inline void
  transform_inits_impl(const stan::io::var_context& context__, VecVar&
                       vars__, std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::serializer<local_scalar_t__> out__(vars__);
    int current_statement__ = 0;
    // suppress unused var warning
    (void) current_statement__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      current_statement__ = 1;
      context__.validate_dims("parameter initialization", "mu", "double",
        std::vector<size_t>{});
      current_statement__ = 2;
      context__.validate_dims("parameter initialization", "tau", "double",
        std::vector<size_t>{});
      current_statement__ = 3;
      context__.validate_dims("parameter initialization", "theta", "double",
        std::vector<size_t>{static_cast<size_t>(J)});
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      local_scalar_t__ mu = DUMMY_VAR__;
      current_statement__ = 1;
      mu = context__.vals_r("mu")[(1 - 1)];
      out__.write(mu);
      local_scalar_t__ tau = DUMMY_VAR__;
      current_statement__ = 2;
      tau = context__.vals_r("tau")[(1 - 1)];
      out__.write_free_lb(0, tau);
      Eigen::Matrix<local_scalar_t__,-1,1> theta =
        Eigen::Matrix<local_scalar_t__,-1,1>::Constant(J, DUMMY_VAR__);
      {
        std::vector<local_scalar_t__> theta_flat__;
        current_statement__ = 3;
        theta_flat__ = context__.vals_r("theta");
        pos__ = 1;
        for (int sym1__ = 1; sym1__ <= J; ++sym1__) {
          stan::model::assign(theta, theta_flat__[(pos__ - 1)],
            "assigning variable theta", stan::model::index_uni(sym1__));
          pos__ = (pos__ + 1);
        }
      }
      out__.write(theta);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  inline void
  get_param_names(std::vector<std::string>& names__, const bool
                  emit_transformed_parameters__ = true, const bool
                  emit_generated_quantities__ = true) const {
    names__ = std::vector<std::string>{"mu", "tau", "theta"};
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {}
  }
  inline void
  get_dims(std::vector<std::vector<size_t>>& dimss__, const bool
           emit_transformed_parameters__ = true, const bool
           emit_generated_quantities__ = true) const {
    dimss__ = std::vector<std::vector<size_t>>{std::vector<size_t>{},
                std::vector<size_t>{},
                std::vector<size_t>{static_cast<size_t>(J)}};
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {}
  }
  inline void
  constrained_param_names(std::vector<std::string>& param_names__, bool
                          emit_transformed_parameters__ = true, bool
                          emit_generated_quantities__ = true) const final {
    param_names__.emplace_back(std::string() + "mu");
    param_names__.emplace_back(std::string() + "tau");
    for (int sym1__ = 1; sym1__ <= J; ++sym1__) {
      param_names__.emplace_back(std::string() + "theta" + '.' +
        std::to_string(sym1__));
    }
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {}
  }
  inline void
  unconstrained_param_names(std::vector<std::string>& param_names__, bool
                            emit_transformed_parameters__ = true, bool
                            emit_generated_quantities__ = true) const final {
    param_names__.emplace_back(std::string() + "mu");
    param_names__.emplace_back(std::string() + "tau");
    for (int sym1__ = 1; sym1__ <= J; ++sym1__) {
      param_names__.emplace_back(std::string() + "theta" + '.' +
        std::to_string(sym1__));
    }
    if (emit_transformed_parameters__) {}
    if (emit_generated_quantities__) {}
  }
  inline std::string get_constrained_sizedtypes() const {
    return std::string("[{\"name\":\"mu\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"tau\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"theta\",\"type\":{\"name\":\"vector\",\"length\":" + std::to_string(J) + "},\"block\":\"parameters\"}]");
  }
  inline std::string get_unconstrained_sizedtypes() const {
    return std::string("[{\"name\":\"mu\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"tau\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"theta\",\"type\":{\"name\":\"vector\",\"length\":" + std::to_string(J) + "},\"block\":\"parameters\"}]");
  }
  // Begin method overload boilerplate
  template <typename RNG> inline void
  write_array(RNG& base_rng, Eigen::Matrix<double,-1,1>& params_r,
              Eigen::Matrix<double,-1,1>& vars, const bool
              emit_transformed_parameters = true, const bool
              emit_generated_quantities = true, std::ostream*
              pstream = nullptr) const {
    const size_t num_params__ = ((1 + 1) + J);
    const size_t num_transformed = emit_transformed_parameters * (0);
    const size_t num_gen_quantities = emit_generated_quantities * (0);
    const size_t num_to_write = num_params__ + num_transformed +
      num_gen_quantities;
    std::vector<int> params_i;
    vars = Eigen::Matrix<double,-1,1>::Constant(num_to_write,
             std::numeric_limits<double>::quiet_NaN());
    write_array_impl(base_rng, params_r, params_i, vars,
      emit_transformed_parameters, emit_generated_quantities, pstream);
  }
  template <typename RNG> inline void
  write_array(RNG& base_rng, std::vector<double>& params_r, std::vector<int>&
              params_i, std::vector<double>& vars, bool
              emit_transformed_parameters = true, bool
              emit_generated_quantities = true, std::ostream*
              pstream = nullptr) const {
    const size_t num_params__ = ((1 + 1) + J);
    const size_t num_transformed = emit_transformed_parameters * (0);
    const size_t num_gen_quantities = emit_generated_quantities * (0);
    const size_t num_to_write = num_params__ + num_transformed +
      num_gen_quantities;
    vars = std::vector<double>(num_to_write,
             std::numeric_limits<double>::quiet_NaN());
    write_array_impl(base_rng, params_r, params_i, vars,
      emit_transformed_parameters, emit_generated_quantities, pstream);
  }
  template <bool propto__, bool jacobian__, typename T_> inline T_
  log_prob(Eigen::Matrix<T_,-1,1>& params_r, std::ostream* pstream = nullptr) const {
    Eigen::Matrix<int,-1,1> params_i;
    return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
  }
  template <bool propto__, bool jacobian__, typename T_> inline T_
  log_prob(std::vector<T_>& params_r, std::vector<int>& params_i,
           std::ostream* pstream = nullptr) const {
    return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
  }
  inline void
  transform_inits(const stan::io::var_context& context,
                  Eigen::Matrix<double,-1,1>& params_r, std::ostream*
                  pstream = nullptr) const final {
    std::vector<double> params_r_vec(params_r.size());
    std::vector<int> params_i;
    transform_inits(context, params_i, params_r_vec, pstream);
    params_r = Eigen::Map<Eigen::Matrix<double,-1,1>>(params_r_vec.data(),
                 params_r_vec.size());
  }
  inline void
  transform_inits(const stan::io::var_context& context, std::vector<int>&
                  params_i, std::vector<double>& vars, std::ostream*
                  pstream__ = nullptr) const {
    vars.resize(num_params_r__);
    transform_inits_impl(context, vars, pstream__);
  }
  inline void
  unconstrain_array(const std::vector<double>& params_constrained,
                    std::vector<double>& params_unconstrained, std::ostream*
                    pstream = nullptr) const {
    const std::vector<int> params_i;
    params_unconstrained = std::vector<double>(num_params_r__,
                             std::numeric_limits<double>::quiet_NaN());
    unconstrain_array_impl(params_constrained, params_i,
      params_unconstrained, pstream);
  }
  inline void
  unconstrain_array(const Eigen::Matrix<double,-1,1>& params_constrained,
                    Eigen::Matrix<double,-1,1>& params_unconstrained,
                    std::ostream* pstream = nullptr) const {
    const std::vector<int> params_i;
    params_unconstrained = Eigen::Matrix<double,-1,1>::Constant(num_params_r__,
                             std::numeric_limits<double>::quiet_NaN());
    unconstrain_array_impl(params_constrained, params_i,
      params_unconstrained, pstream);
  }
};
}
using stan_model = _8schools_model_namespace::_8schools_model;
#ifndef USING_R
// Boilerplate
stan::model::model_base&
new_model(stan::io::var_context& data_context, unsigned int seed,
          std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
stan::math::profile_map& get_stan_profile_data() {
  return _8schools_model_namespace::profiles__;
}
#endif