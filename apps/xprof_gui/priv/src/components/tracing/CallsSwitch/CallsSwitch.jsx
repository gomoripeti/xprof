import PropTypes from 'prop-types';
import React from 'react';

const defaultProps = {
  disabled: false,
  collecting: false,
};

const propTypes = {
  monitored: PropTypes.shape({
    graph_type: PropTypes.string,
    mfa: PropTypes.arrayOf(PropTypes.any),
    query: PropTypes.string,
  }).isRequired,
  disabled: PropTypes.bool,
  collecting: PropTypes.bool,
  toggleCallsTracing: PropTypes.func.isRequired,
};

const CallsSwitch = ({
  monitored, disabled, collecting, toggleCallsTracing,
}) => (
  <span>
    <button
      type="submit"
      onClick={(e) => {
        e.preventDefault();
        toggleCallsTracing(monitored);
      }}
      className={collecting ? 'btn btn-danger' : 'btn btn-success'}
      disabled={disabled}
    >
      {collecting ? 'Stop' : 'Start'}
    </button>
  </span>
);

CallsSwitch.defaultProps = defaultProps;
CallsSwitch.propTypes = propTypes;

export default CallsSwitch;
